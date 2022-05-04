DROPBOX_URL_BODY <- "dropboxapi.com/2/"
DROPBOX_MEGABYTE_FILE_SIZE_LIMIT <- 150

# httr default functions to parse different filetypes
# See https://httr.r-lib.org/reference/content.html
FILETYPE_PARSERS <- list(
  "html" = xml2::read_html,
  "xml" = xml2::read_xml,
  "csv" = readr::read_csv,
  "tsv" = readr::read_tsv,
  "json" = jsonlite::fromJSON,
  "jpg" = jpeg::readJPEG,
  "png" = png::readPNG
)

# httr can guess this parsing method automatically,
# but documentation recommends you specify it yourself
parse_json_response <- function(response) {
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
}

# Get the appropriate function to parse a given filetype by extension, with an optional fallback if none match
handle_extension <- function(extension, choices = FILETYPE_PARSERS, fallback = identity) {
  do.call(switch, c(extension, choices, fallback))
}

# Returns an oauth 2.0 token needed to authenticate app requests
dropbox_authenticate <- function(app_key = "u6y3vt6q662jl67", app_secret = "pyil41cirt6l917", token_path = NULL, refresh = FALSE) {
  dropbox <- httr::oauth_endpoint(
    authorize = "https://www.dropbox.com/oauth2/authorize",
    access = "https://api.dropbox.com/oauth2/token"
  )

  dropbox_app <- httr::oauth_app("dropbox", app_key, app_secret)
  if (refresh) {
    query_authorize_extra <- list(token_access_type = "offline")
  } else {
    query_authorize_extra <- list()
  }
  dropbox_token <- httr::oauth2.0_token(
    endpoint = dropbox,
    app = dropbox_app, cache = FALSE, scope = c(
      "files.content.read",
      "files.content.write", "sharing.write",
      "sharing.read"
    ), query_authorize_extra = query_authorize_extra
  )

  if (!is.null(token_path)) {
    if (!validate_path(token_path)) {
      stop(token_path, " does not exist, or you lack write permission for it")
    }
    saveRDS(dropbox_token, token_path)
  }
  dropbox_token
}
# Confirm user has write permission for some path
# (i.e., write permission for directory)
validate_path <- function(path) {
  if (!file.exists(path) || !file.info(path)[["isdir"]]) {
    path <- dirname(path)
  }
  file.access(path, 2L) == 0L
}

# If a token is a string, treat it as a path and try to load it; if an R object, confirm it has correct class
dropbox_token_get <- function(dropbox_token = ".httr-oauth") {
  if (is.character(dropbox_token)) {
    if (!file.exists(dropbox_token)) {
      stop("Could not read ", dropbox_token)
    }
    dropbox_token <- readRDS(dropbox_token)
    if (!inherits(dropbox_token, "Token2.0")) {
      stop("Token is not valid")
    }
  }
  dropbox_token
}

# Ensure path starts with / and does not end with /,
# as Dropbox requires
dropbox_correct_path <- function(paths) {
  if (any(grepl("^\\./", paths))) {
    stop("Dropbox does not support relative paths")
  }

  if (paths == "/") stop("The root folder is unsupported")
  gsub("[/ ]+$", "", gsub("^([^/])", "/\\1", paths))
}

# Lists paths in a Dropbox folder, optionally limitited to just files or folders.
# Unavoidably slow in the latter case if a target type is specified, because that requires recursing
# through folders at the R level
# TODO possibly enable recursion for folders not matching pattern
dropbox_list_files <- function(dropbox_token,
                               dropbox_path = "",
                               recursive = FALSE,
                               full_paths = FALSE,
                               pattern = ".*",
                               target = c("all", "files", "folders")) {
  if (full_paths && !recursive) recursive <- TRUE
  dropbox_token <- dropbox_token_get(dropbox_token)
  # API expects all paths prepended with /
  dropbox_path <- dropbox_correct_path(dropbox_path)
  if (dropbox_path != "" && dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = TRUE) == "file") stop(sQuote(dropbox_path), " is a file, not a folder")
  target <- match.arg(target)
  req <- form_request(
    url = form_url("files", "list_folder"),
    body =
      list(path = dropbox_path, recursive = FALSE),
    encode = "json",
    dropbox_token = dropbox_token
  )
  response <- eval(req)
  httr::stop_for_status(response)
  current <- parse_json_response(response)
  continue_url <- form_url("files", "list_folder", "continue")
  continue_request <- form_request(encode = "json", url = continue_url, body = list(cursor = cursor))

  # Iterate over pages of response
  while (current[["has_more"]]) {
    cursor <- current[["cursor"]]
    additional <- eval(continue_request)
    httr::stop_for_status(additional)
    additional <- parse_json_response(additional)
    # Update with new data from next page
    current[["entries"]] <- rbind(current[["entries"]], additional[["entries"]])
    current[c("has_more", "cursor")] <- additional[c("has_more", "cursor")]
  }
  paths <- current[[c("entries", "path_display")]]
  out <- if (full_paths) paths else current[[c("entries", "name")]] # unlist(c(sapply(
  # )))
  names(out) <- NULL
  # Filter files that match pattern
  out <- out[grepl(pattern, out)]
  types <- current[[c("entries", ".tag")]]
  new_paths <- NULL
  if (recursive && length(folders <- paths[types == "folder"]) > 0) {
    # Descend into each folder and retrieve its contents of the correct type
    for (path in folders) {
      new_paths <- c(new_paths, unlist(dropbox_list_files(dropbox_token, path,
        recursive = TRUE, full_paths = full_paths, target = target
      )))
    }
  }
  if (target != "all") {
    out <- out[types == sub("s$", "", target)]
  }
  c(out, new_paths)
}
# Generate expression containing API call from parts. Like all generated code, no guarantee of validity
form_request <- function(...,
                         type = httr::POST,
                         url,
                         dropbox_token = dropbox_token,
                         body = NULL) {
  dots <- match.call(expand.dots = FALSE)[["..."]]
  type <- substitute(type)
  dropbox_token <- substitute(dropbox_token)
  body <- substitute(body)
  headers <- substitute(headers)

  # subs <- subs[lengths(subs) > 0]
  args <- c(
    type,
    url = url,
    substitute(httr::config(token = dropbox_token)),
    body = substitute(body)
  )
  if (length(dots) > 0) {
    args <- append(args, dots)
  }
  as.call(args)
}


# Form URL endpoint from parts (NB no trailing slashes)
form_url <- function(..., head = "api.", end = DROPBOX_URL_BODY) {
  glue::glue("https://{head}{end}{paste(..., sep = '/')}")
}
#

dropbox_create_folder <- function(dropbox_token, dropbox_path) {
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  req <- form_request(
    url = form_url("files", "create_folder_v2"),
    body = list(path = dropbox_path),
    encode = "json"
  )
  response <- eval(req)
  httr::stop_for_status(response)
  # if (status <- httr::status_code(response) == 409) {
  # httr::message_for_status(response, sprintf(" create %s. It most likely already exists\n", dropbox_path))
  # } else if (status != 200) {
  # cat("Failed to create", dropbox_path, "with error code", status, "\n")
  # }
}

# Download a single file from Dropbox storage.
# Dots contain additional arguments to content parser function
dropbox_download_file <- function(dropbox_token,
                                  dropbox_path,
                                  save_path = basename(dropbox_path),
                                  save_file = TRUE,
                                  overwrite = FALSE,
                                  content_parser = handle_extension(tools::file_ext(dropbox_path)),
                                  ...) {
  if (save_file && !validate_path(save_path)) {
    stop(save_path, " is not a valid path, or you lack write permission for it")
  }
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)

  # Determine whether target path is file or folder
  if (isFALSE(dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = TRUE) == "file")) {
    stop(dropbox_path, " is not a file")
  }
  if (save_file && !overwrite && file.exists(save_path)) {
    stop(save_path, " already exists and overwrite = FALSE")
  }

  req <- form_request(
    httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = dropbox_path), auto_unbox = TRUE)
    ),
    if (save_file) httr::write_disk(save_path, overwrite),
    url = form_url("files", "download", head = "content.")
  )

  response <- eval(req)
  httr::stop_for_status(response)
  if (save_file) {
    out <- TRUE
  } else {
    content_parser <- content_parser
    out <- tryCatch(
      content_parser(httr::content(response, as = "raw"), ...),
      error = function(e) stop("Parsing of ", sQuote(dropbox_path), " failed")
    )
  }
  out
}

# To unzip with no arguments, pass empty list to zip_args
dropbox_download_folder <- function(dropbox_token, dropbox_path,
                                    unzip_folder = TRUE,
                                    zip_path = tempfile(), unzip_args = list(exdir = ".", unzip = "unzip", overwrite = TRUE)) {
  if (!is.null(unzip_args$exdir) && !validate_path(unzip_args$exdir)) {
    stop(unzip_args$exdir, " is not a valid path, or you lack write permission for it")
  }
  # Determine whether target path is file or folder
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  req <- form_request(
    httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = dropbox_path), auto_unbox = TRUE)
    ),
    httr::write_disk(zip_path, isTRUE(unzip_args$overwrite)),
    url = form_url("files", "download_zip", head = "content.")
  )

  response <- eval(req)
  httr::stop_for_status(response)

  if (unzip_folder) {
    tryCatch(
      do.call(unzip, append(list(zipfile = zip_path), unzip_args)),
      error = function(e) stop("Error unzipping ", save_path, ": ", as.character(e), "\n")
    )
  } else {
    message(sQuote(dropbox_path), " successfully downloaded to ", zip_path)
  }
  TRUE
}

# Get file sizes from local paths. Needed because file.size does not work for .jpeg.
file_size <- function(paths, use_bytes = FALSE) {
  as.integer(system(paste(
    "du", paste(shQuote(paths), collapse = " "),
    if (use_bytes) "-b",
    "| cut -f1"
  ), intern = TRUE))
}

# Determines if path is file or folder, returning NA if it doesn't exist. Can also be specified to raise error on failure
dropbox_path_info <- function(dropbox_token,
                              dropbox_path,
                              error_on_failure = FALSE) {
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  req <- form_request(body = list(path = dropbox_path), url = form_url("files", "get_metadata"), encode = "json")
  response <- eval(req)
  if ((status <- httr::status_code(response)) == 409) {
    if (error_on_failure) stop(sQuote(dropbox_path), " does not exist") else out <- NA
  } else if (status >= 400) {
    stop("HTTP error ", status)
  } else {
    out <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))[[".tag"]]
  }
  out
}

# Raise exceptions if file and/or parent folder don't exist, depending on previous arguments
handle_upload_options <- function(dropbox_token, dropbox_path, overwrite = FALSE, make_parent_folders) {
  parent <- dirname(sub("^/", "", dropbox_path))
  # If parent folder doesn't exist and file not at top level, create one if specified
  if (parent != "." && is.na(dropbox_path_info(dropbox_token, parent, error_on_failure = FALSE))) {
    if (make_parent_folders) {
      dropbox_create_folder(dropbox_token, parent)
    } else {
      stop("Parent folder of ", dropbox_path, "does not exist, but make_parent_folders = FALSE")
    }
  } else {
    # If file already exists and overwrite not specified, warn and continue
    # Not a stop for case of uploading files in folder; then, just skipping preexisting files would be desirable
    # if (!is.na(type <- dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = FALSE))) {
    #  message(dropbox_path, "already exists, and overwrite = FALSE")
    #  return(FALSE)
    # }
  }

  # If all good, create folder if necessary
  # if (type == "folder") {
  # dropbox_create_folder(dropbox_token, dropbox_path)
  # }
  TRUE
}

dropbox_upload_files <- function(dropbox_token,
                                 local_paths, dropbox_path,
                                 overwrite = TRUE,
                                 make_parent_folders = TRUE) {
  stopifnot(
    "File does not exist" = all(file.exists(local_path)),
    "Path is a directory, not a file" = !any(dir.exists(local_path)),
    "Target path is a file, not a folder" = isTRUE(dropbox_path_info(dropbox_token, dropbox_path) != "folder")
  )
  # TODO handle nonexistent target directory
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  # Stop on name conflict if overwrite = FALSE
  handle_upload_options(dropbox_token, dropbox_path, overwrite = overwrite, make_parent_folders = make_parent_folders)

  # Form separate request templates
  urls <- form_url("files", c("upload_session/start", "upload_session/append", "upload_session/finish"), head = "content.")

  # Max 1000 entries per request
  session_templates <- list(
    start =
      form_request(httr::add_headers(
        `Dropbox-API-Arg` = jsonlite::toJSON(list(close = FALSE), auto_unbox = TRUE),
        url = urls[1],
        body = httr::upload_file(path = dropbox_paths[1], type = "application/octet-stream")
      )),
    append = form_request(httr::add_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(cursor = cursor),
      auto_unbox = TRUE
    )),
    url = urls[2],
    body = httr::upload_file(path = dropbox_paths[i], type = "application/octet-stream")
    ),
    finish = form_request(httr::add_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(cursor = cursor),
      auto_unbox = TRUE
    )),
    url = urls[2],
    body = httr::upload_file(path = dropbox_paths[i], type = "application/octet-stream")
    )
  )

  response <- eval(session_templates[["start"]])
  httr::stop_for_status(initial_response)
  # need to do batched upload if bigger than 150 MB
  upload_size <- file_size(local_paths)
  dropbox_paths <- file.path(basename(local_paths), dropbox_path)

  # Remove files bigger than absolute size limit
  if (length(too_large <- which(local_paths[upload_size > 350000])) > 0) {
    sapply(sQuote(dropbox_paths[too_large]), message, " exceeds absolute upload size limit of 350 gigabytes")
    dropbox_paths <- dropbox_paths[-too_large]
  }
  for (i in seq_along(dropbox_paths)[-1]) {
    req <- form_request(httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = dropbox_path), auto_unbox = TRUE)
    ),
    body = httr::upload_file(local_path, type = "application/octet-stream"),
    url = form_url("files", "upload", head = "content.")
    )
    response <- eval(req)
    httr::stop_for_status(response)
  }
}

# Size in bytes - default is recommended 8 MB
# Unfinished - do not use
dropbox_upload_large_file <- function(dropbox_token, local_path, dropbox_path,
                                      overwrite = FALSE, chunk_size = 8 * 1024^2) {
  stop("Not implemented")
  if (!file.exists(local_path)) stop(local_path, " does not exist")
  if (!overwrite && !is.na(dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = FALSE))) stop(dropbox_path, " already exists, but overwrite == FALSE")
  size <- file_size(local_path)
  if (size < 150 * 1024^2) message("For files smaller than 150 megabytes, batch uploading is unnecessary")

  urls <- form_url("files", c("upload_session/start", "upload_session/append", "upload_session/finish"),
    head = "content."
  )
  session_templates <- list(
    start =
      form_request(httr::add_headers(
        `Dropbox-API-Arg` = jsonlite::toJSON(list(close = FALSE), auto_unbox = TRUE)
      ),
      url = urls[1],
      ),
    append = form_request(httr::add_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(cursor = list(
      session_id = session_id, offset = offset[i],
      close = FALSE
    )),
    auto_unbox = TRUE
    )),
    url = urls[2],
    body = httr::upload_file(path = chunks[i], type = "application/octet-stream")
    ),
    finish = form_request(httr::add_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(
      cursor = list(session_id = session_id, offset = size),
      commit = list(
        path = tmp_path, mode = "add", autorename = TRUE,
        mute = FALSE, strict_conflict = FALSE
      )
    ),
    auto_unbox = TRUE
    )),
    url = urls[3],
    )
  )

  split_dir <- tempdir(check = TRUE)
  file.copy(local_path, split_dir)
  system2("split", args = c("--suffix-length=3", shQuote(paste0("-b", chunk_size)), "-d", shQuote(file.path(split_dir, local_path))))
  tmp_path <- file.path(split_dir, local_path)

  # Split file into chunks of given size, then append eacch chunk to the
  # initial upload, using chunk sizes as offsets
  chunks <- sort(list.files(split_dir, pattern = "x\\d{3}", full.names = TRUE))
  session_id <- eval(session_templates[["start"]])
  httr::stop_for_status(session_id)
  session_id <- parse_json_response(response)[[c("entries", "session_id")]]
  offsets <- seq(from = 0, to = size %/% chunk_size, by = chunk_size)

  for (i in seq_along(chunks)) {
    current <- eval(session_templates[["append"]])
    httr::stop_for_status("current")
  }
  final_response <- eval(session_templates[["finish"]])
  httr::stop_for_status(final_response)
}

dropbox_upload_file <- function(dropbox_token,
                                local_path, dropbox_path,
                                overwrite = TRUE,
                                make_parent_folders = TRUE) {
  stopifnot(
    "File does not exist" = file.exists(local_path),
    "Path is a directory, not a file" = !dir.exists(local_path)
  )
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  # Stop on name conflict if overwrite = FALSE
  handle_upload_options(dropbox_token, overwrite = overwrite, make_parent_folders = make_parent_folders)

  # need to do batched upload if bigger than 150 MB
  upload_size <- file_size(local_path) / 1000000
  if (upload_size >= 350 * 10^9) { # 350 GB
    message(sQuote(path), " exceeds absolute upload size limit of 350 gigabytes")
    return()
  } else if (upload_size >= DROPBOX_MEGABYTE_FILE_SIZE_LIMIT) {
    message(sQuote(path), "is larger than maximum upload size of 150 megabytes")
    return()
  } else {
    req <- form_request(httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = dropbox_path), auto_unbox = TRUE)
    ),
    body = httr::upload_file(local_path, type = "application/octet-stream"),
    url = form_url("files", "upload", head = "content.")
    )
    response <- eval(req)
    httr::stop_for_status(response)
  }
}

# Master uploading function, automatically dispatching on whether path is directory or file
dropbox_upload <- function(dropbox_token, local_path, dropbox_path, overwrite = TRUE, make_parent_folders = TRUE) {
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  # Stop on name conflict if overwrite = FALSE
  handle_upload_options(dropbox_token, dropbox_path, overwrite = overwrite, make_parent_folders = make_parent_folders)
  if (!file.exists(local_path)) {
    stop(local_path, "does not exist")
  }
  # Recursive case
  if (dir.exists(local_path)) {
    for (path in list.files(local_path, include.dirs = TRUE, all.files = TRUE)) {
      dropbox_upload(
        dropbox_token = dropbox_token,
        local_path = path, dropbox_path = file.path(dropbox_path, basename(file)), overwrite = overwrite, make_parent_folders = make_parent_folders
      )
    }
    # Base case
  } else {
    # need to do batched upload if bigger than 150 MB
    if (file_size(local_path) / 1000000 >= DROPBOX_MEGABYTE_FILE_SIZE_LIMIT) {
      message(sQuote(path), "is larger than maxium upload size of 150 megabytes")
      return()
    } else {
      req <- form_request(httr::add_headers(
        `Dropbox-API-Arg` = jsonlite::toJSON(list(path = dropbox_path), auto_unbox = TRUE)
      ),
      body = httr::upload_file(local_path, type = "application/octet-stream"),
      url = form_url("files", "upload", head = "content.")
      )
    }
    response <- eval(req)
    httr::stop_for_status(response)
    message("Successfully uploaded ", sQuote(local_path))
  }
}


# Deletes both files and folders. If recursive = FALSE, throws error if a folder contains another folder, a la GNU rm
dropbox_remove <- function(dropbox_token, dropbox_path, recursive = FALSE) {
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  # Stop if folder contains a folder
  if (!recursive && isTRUE(dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = TRUE) == "folder")) {
    files <- dropbox_correct_path(file.path(dropbox_path, dropbox_list_files(dropbox_token, dropbox_path, recursive = FALSE)))
    for (path in files) {
      if (isTRUE(dropbox_path_info(dropbox_token, path, error_on_failure == FALSE) == "folder")) {
        stop(path, " is a folder and recursive = FALSE")
      }
    }
  }
  req <- form_request(
    body = list(path = dropbox_path),
    url = form_url("files", "delete_v2"),
    encode = "json"
  )
  response <- eval(req)
  httr::stop_for_status(response)
}


# Recursively uploads directories within a local directory, creating parent directories if necessary
dropbox_upload_folder <- function(dropbox_token,
                                  local_path, dropbox_path,
                                  overwrite = FALSE, make_parent_folders = TRUE,
                                  hidden_files = FALSE,
                                  verbose = TRUE,
                                  pattern = ".*") {
  dropbox_token <- dropbox_token_get(dropbox_token)
  dropbox_path <- dropbox_correct_path(dropbox_path)
  handle_upload_options(dropbox_token, dropbox_path,
    overwrite = overwrite, make_parent_folders = make_parent_folders
  )

  # Substitute file names into this
  template <- form_request(httr::add_headers(
    `Dropbox-API-Arg` = jsonlite::toJSON(list(
      path = upload_path
    ),
    auto_unbox = TRUE
    )
  ),
  body = httr::upload_file(new_path, type = "application/octet-stream"),
  url = form_url("files", "upload", head = "content.")
  )
  # Must create new folder if it doesn't already exist
  # list.files does not work in my tests, requiring setdiff here
  all_files <- list.files(path = local_path, pattern = pattern, full.names = FALSE, all.files = hidden_files, include.dirs = TRUE, recursive = FALSE)
  types <- file.info(file.path(local_path, all_files), extra_cols = FALSE)$isdir
  dirs <- all_files[types]
  files <- all_files[!types]

  dropbox_type <- dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = FALSE)
  if (is.na(dropbox_type)) {
    dropbox_create_folder(dropbox_token, dropbox_path)
  } else if (dropbox_type != "folder") {
    stop("Target Dropbox path ", dropbox_path, " exists but is not a folder")
    # Exclude extant files (including folders) in this folder if overwrite == FALSE
  }
  if (!overwrite) {
    files <- setdiff(files, dropbox_list_files(dropbox_token, dropbox_path))
  }

  for (path in files) {
    new_path <- file.path(local_path, path)
    upload_path <- file.path(dropbox_path, path)
    if (file_size(new_path) / (1000000) >= DROPBOX_MEGABYTE_FILE_SIZE_LIMIT) {
      message(sQuote(path), " exceeds the maximum upload size of 150 megabytes")
    } else if (overwrite || is.na(dropbox_path_info(dropbox_token, upload_path, error_on_failure = FALSE))) {
      # Hadley Wickham's trick to susbtitute into an expression indirectly
      request <- eval(substitute(substitute(template)))
      response <- eval(request)
      httr::stop_for_status(response)
      if (verbose) message("Successfully uploaded ", sQuote(new_path))
    }
  }

  # No need for parents, since the parent is already created
  for (path in dirs) {
    dropbox_upload_folder(
      dropbox_token = dropbox_token,
      local_path = file.path(local_path, path),
      dropbox_path = file.path(dropbox_path, path),
      overwrite = overwrite,
      hidden_files = hidden_files,
      verbose = verbose,
      make_parent_folders = FALSE,
      pattern = pattern
    )
  }
  TRUE
}

# Return a shared link to a file or folder.
# Generates informative failure if link already exists.
dropbox_create_share_link <- function(dropbox_token, dropbox_path) {
  # Currently not working due to permission error
  dropbox_path <- dropbox_correct_path(dropbox_path)
  dropbox_token <- dropbox_token_get(dropbox_token)
  if (is.na(dropbox_path_info(dropbox_token, dropbox_path, error_on_failure = FALSE))) stop(sQuote(dropbox_path), " does not exist")
  req <- form_request(
    url = form_url(
      "sharing",
      "create_shared_link_with_settings"
    ),
    body = list(
      path = dropbox_path,
      settings = list(allow_download = TRUE)
    ),
    encode = "json"
  )
  response <- eval(req)
  parsed <- parse_json_response(response)
  if ("error_summary" %in% names(parsed) && grepl("shared_link", parsed[["error_summary"]])) {
    stop("Shared link already created for ", sQuote(dropbox_path))
  }
  httr::stop_for_status(response)
  parsed[["url"]]
}
