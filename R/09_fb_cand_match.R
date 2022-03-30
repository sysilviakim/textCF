# Setup ========================================================================
source(here::here("R", "utilities.R"))
vec <- c(senate = "senate", house = "house")

# Import data ==================================================================
fb_list <- vec %>%
  map(~ loadRData(here("data", "tidy", paste0("fb_", .x, "_merged.Rda")))) %>%
  map(~ .x %>% rename(adlib_id = id))

# Candidate list ===============================================================
cand_list <- vec %>%
  map(
    ~ read_csv(
      here("data", "raw", "fb", paste0("fb-", .x, ".csv")),
      col_types = cols(.default = "c")
    ) %>%
      select(where(~ !all(is.na(.x))))
  ) %>%
  map(dedup)

cong_complete <- loadRData("data/tidy/cong_complete.Rda") %>%
  map(
    ~ .x %>%
      clean_names() %>%
      select(
        -year, -contains("stage"), -contains("special"),
        -contains("candidatevotes"), -contains("totalvotes"), -contains("party")
      ) %>%
      select(candidate, everything())
  ) %>%
  ## dedup here is insufficient, because sometimes, there are multiple
  ## display_name_brandings, id_brandings, default, image_url, mobile_image_url
  map(
    ~ .x %>%
      group_by(candidate) %>%
      filter(!(n() > 1 & is.na(header_url))) %>%
      ungroup() %>%
      select(-display_name_brandings, -id_brandings, -default, -image_url) %>%
      select(
        -contains("mobile_image"), -contains("background_image"),
        -contains("header_image"), -contains("image_href"),
        -contains("arrangement"), -contains("public_"), -contains("file_"),
        -contains("shared_title"), -contains("shared_contribution_blurb"),
        -contains("full_background"), -contains("reusable")
      ) %>%
      dedup()
  )

assert_that(!any(duplicated(cong_complete$senate$candidate)))
assert_that(!any(duplicated(cong_complete$house$candidate)))

fb_list %>% map_dbl(nrow)
# senate  house
# 344536 260934

cand_list %>% map_dbl(nrow)
# senate  house
#     75    868

cong_complete %>% map_dbl(nrow)
# senate  house
#    144    817

# Creating initial fb_matched object ===========================================

## FB original data
fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>%
      ## Joining, by = "candidate"
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )

# Identifying which candidates still have NAs ==================================

## Converting Senate candidates to upper -- this will reduce number of NAs in
## fb_matched[['senate']] from 41 to 15 (see further down in this section)
fb_list[["senate"]]$candidate <- toupper(fb_list[["senate"]]$candidate)

# The new version of fb_matched has removed most of the NAs! Let's figure out
# a list of who still has an NA value for incumbency:
matched_house_nas <- as.data.frame(cbind(fb_matched[["house"]]$candidate,
                                         fb_matched[["house"]]$inc))
matched_house_nas <- unique(matched_house_nas)
names(matched_house_nas) <- c('candidate', 'inc')
matched_house_nas <- matched_house_nas[is.na(matched_house_nas$inc),]
# 43 NAs...
save(matched_house_nas, file = here("data", "raw", "fb_matched_house_nas.Rda"))

# Now, for the Senate:
matched_senate_nas <- as.data.frame(cbind(fb_matched[["senate"]]$candidate,
                                          fb_matched[["senate"]]$inc))
matched_senate_nas <- unique(matched_senate_nas)
names(matched_senate_nas) <- c('candidate', 'inc')
matched_senate_nas <- matched_senate_nas[is.na(matched_senate_nas$inc),]
# Tried this before running the line currently at line 67 -- was 41 NAs, not 15
save(matched_senate_nas, file = here("data", "raw", 
                                     "fb_matched_senate_nas.Rda"))

# Diagnosing Issues for Candidates Missing Data ================================

## Senate:
# Using matched_senate_nas, cong_complete[["senate]], and list_sen (below), ID
# which ones are mismatches and which ones are the results of something else
list_sen <- as.data.frame(unique(fb_list[["senate"]]$candidate))
## Not in one of the datasets:
## Alex Padilla: Not in cong_complete
### However, this is not concerning -- he was not running for Senate in 2020,
### but was appointed after Kamala Harris had to resign her Senate seat
## Pat Toomey: not in cong_complete...not sure why not, this is more concerning
## Rand Paul: not in cong_complete...

## Senate Mismatches:
# Bob Casey: BOB CASEY in list, ROBERT P CASEY JR in cong_complete
# Chris Murphy: CHRIS MURPHY in list, CHRISTOPHER S MURPHY in cong_complete
# Chuck Schumer: CHUCK SCHUMER in list, CHARLES E. SCHUMER in cong_complete
# Elizabeth Warren: ELIZABETH WARREN in list, ELIZABETH A. WARREN ~
# John Thune: JOHN THUNE ~, JOHN R. THUNE ~
# Kirsten Gillibrand: KIRSTEN GILLIBRAND ~, KIRSTEN E. GILLIBRAND ~
# Martin Heinrich: MARTIN HEINRICH ~, MARTIN T HEINRICH ~
# Mazie Hirono: MAZIE HIRONO ~, MAZIE K. HIRONO ~
# Michael Bennet: MICHAEL BENNET ~, MICHAEL F. BENNET ~
# Patrick Leahy: PATRICK LEAHY ~, PATRICK J. LEAHY ~
# Roger Wicker: ROGER WICKER ~, ROGER F. WICKER ~
# Todd Young: TODD YOUNG ~, TODD C. YOUNG ~

# The plan: case_whens to bring these in -- turn cong_complete names into list
# versions for computational efficiency, as there are a lot more rows in fb_list
# than there are in cong_complete
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "ROBERT P CASEY JR"] <- "BOB CASEY"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "CHRISTOPHER S MURPHY"] <- "CHRIS MURPHY"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "CHARLES E. SCHUMER"] <- "CHUCK SCHUMER"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "ELIZABETH A. WARREN"] <- "ELIZABETH WARREN"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "JOHN R. THUNE"] <- "JOHN THUNE"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "KIRSTEN E. GILLIBRAND"] <- "KIRSTEN GILLIBRAND"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "MARTIN T HEINRICH"] <- "MARTIN HEINRICH"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "MAZIE K. HIRONO"] <- "MAZIE HIRONO"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "MICHAEL F. BENNET"] <- "MICHAEL BENNET"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "PATRICK J. LEAHY"] <- "PATRICK LEAHY"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "ROGER F. WICKER"] <- "ROGER WICKER"
cong_complete[["senate"]]$candidate[
  cong_complete[["senate"]]$candidate == "TODD C. YOUNG"] <- "TODD YOUNG"

# Reran to double-check -- this worked! FOr all but the ones who aren't in
# cong_complete, the variables are filled in.

## House:
# Using matched_house_nas, cong_complete[["house"]], and list_house (below), ID
# which ones are mismatches and which ones are the results of something else
list_house <- as.data.frame(unique(fb_list[["house"]]$candidate))

## Not in one of the datasets:
# Ben Gibson: Not in cong_complete
# Ricky de la Fuente: Not in cong_complete
# Christy Smith: Not in cong_complete
# Dan Feehan: Not in cong_complete
# David Scott: Not in cong_complete
# David Torres: Not in cong_complete
# Tawnja Zahradka:  Not in cong_complete, TAWNJA ZAHRADKA in list
# Zach Raknerud: Not in cong_complete, ZACH RAKNERUD in list
# Tracy Jennings: Not in cong_complete, TRACY JENNINGS in list
# Diane E. Mitsch Busch: Not in cong_complete, DIANE E. MITSCH BUSH in list
# Frank D. Lucas: Not in cong_complete, FRANK D. LUCAS in list
# John Briscoe: Not in cong_complete, JOHN BRISCOE in list
# Kevin Van Ausdal: ____ in cong_complete, KEVIN VAN AUSDAL in list
# Liz Johnson: ____ in cong_complete, LIZ JOHNSON in list
# Mark Razzoli: ____ in cong_complete, MARK RAZZOLI in list
# Michael San Nicolas: ____ in cong_complete, MICHAEL F. Q. SAN NICOLAS in list
# Quinn Nystrom: ____ in cong_complete, QUINN NYSTROM in list
# Randy Weber: ____ in cong_complete, RANDY K. WEBER, SR. in list
# Tim Kelly: ____ in cong_complete, TIM KELLY in list

## Mismatches:
# Scotty Robinson: SCOTTY ROBINSON in cong_complete, ¬ìSCOTTY¬î ROBINSON in list
# Andre Carson: ANDRé CARSON in cong_complete, ANDR√© CARSON in list
# Angelica Maria Duenas: ANGéLICA MARIA DUEñAS in cong_complete, ANG√©LICA MARIA DUE√±AS in list
# Bill Pascrell: BILL PASCRELL, JR. in cong_complete, BILL PASCRELL,  JR. in list
### The two Bills are not identical -- the space is different
# C. Antonio Delgado: ANTONIO DELGADO in cong_complete, C. ANTONIO DELGADO in list
# Dartanyon Dar Williams: DARTANYON DAW WILLIAMS in cong_complete, DARTANYON ¬ìDAW¬î WILLIAMS in list
# Earl Buddy Carter: EARL L. BUDDY CARTER in cong_complete, EARL L. ¬ìBUDDY¬î CARTER in list
# George Kelly:  GEORGE J. KELLY, JR. in cong_complete, GEORGE J. KELLY,  JR. in list
### Same as with Bill Pascrells -- different space length
# Georgette Gomez: GEORGETTE GóMEZ in cong_complete, GEORGETTE G√≥MEZ in list
# Glenn Thompson: GLENN W. THOMPSON, JR. in cong_complete, GLENN W. THOMPSON,  JR. in list
# Hillary O'Connor Mueri: HILLARY O'CONNOR MUERI in cong_complete, HILLARY O¬íCONNOR MUERI in list
# Jim Pruden: JAMES JIM PRUDEN in cong_complete, JAMES ¬ìJIM¬î PRUDEN in list
# Jesus Chuy Garcia: JESúS G. CHUY GARCíA in cong_complete, JES√∫S G. ¬ìCHUY¬î GARC√≠A in list
# John W. Collick: JOHN W. COLLICK, JR. in cong_complete, JOHN W. COLLICK,  JR. in list
# Linda T Sanchez: LINDA T. SáNCHEZ in cong_complete, LINDA T. S√°NCHEZ in list
# Michael F. Doyle: MICHAEL F. DOYLE, JR. in cong_complete, MICHAEL F. DOYLE,  JR. in list
# Nydia Velazquez: NYDIA M. VELáZQUEZ in cong_complete, NYDIA M. VEL√°ZQUEZ in list
# Pat Hackett: PATRICIA PAT HACKETT in cong_complete, PATRICIA ¬ìPAT¬î HACKETT in list
# Raul Griijalva: RAúL M. GRIJALVA in cong_complete, RA√∫L M. GRIJALVA in list
# Bobby Scott: ROBERT C. BOBBY SCOTT in cong_complete, ROBERT C. ¬ìBOBBY¬î SCOTT in list
# Bob Good: ROBERT G. BOB GOOD in cong_complete, ROBERT G. ¬ìBOB¬î GOOD in list
# Candy Christophe: SANDRA CANDY CHRISTOPHE in cong_complete, SANDRA ¬ìCANDY¬î CHRISTOPHE in list
# Tom O'Halleran: TOM O'HALLERAN in cong_complete, TOM O¬íHALLERAN in list
# Bill Olson: WILLIAM P. BILL OLSON in cong_complete, WILLIAM P. ¬ìBILL¬î OLSON in list

# While it will be more computationally intensive, the names in cong_complete
# are generally more coherent; so, the names in cand_list will be replaced with
# those in cong_complete:
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "¬ìSCOTTY¬î ROBINSON"] <- "SCOTTY ROBINSON"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "ANDR√© CARSON"] <- "ANDRE CARSON"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "ANG√©LICA MARIA DUE√±AS"] <- "ANGéLICA MARIA DUEñAS"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "BILL PASCRELL,  JR."] <- "BILL PASCRELL, JR."
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "C. ANTONIO DELGADO"] <- "ANTONIO DELGADO"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "DARTANYON ¬ìDAW¬î WILLIAMS"] <- "DARTANYON DAW WILLIAMS"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "EARL L. ¬ìBUDDY¬î CARTER"] <- "EARL L. BUDDY CARTER"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "GEORGE J. KELLY,  JR."] <- "GEORGE J. KELLY, JR."
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "GEORGETTE G√≥MEZ"] <- "GEORGETTE GóMEZ"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "GLENN W. THOMPSON,  JR."] <- "GLENN W. THOMPSON, JR."
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "HILLARY O¬íCONNOR MUERI"] <- "HILLARY O'CONNOR MUERI"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "JAMES ¬ìJIM¬î PRUDEN"] <- "JAMES JIM PRUDEN"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "JES√∫S G. ¬ìCHUY¬î GARC√≠A"] <- "JESúS G. CHUY GARCíA"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "JOHN W. COLLICK,  JR."] <- "JOHN W. COLLICK, JR."
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "LINDA T. S√°NCHEZ"] <- "LINDA T. SáNCHEZ"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "MICHAEL F. DOYLE,  JR."] <- "MICHAEL F. DOYLE, JR."
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "NYDIA M. VEL√°ZQUEZ"] <- "NYDIA M. VELáZQUEZ"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "PATRICIA ¬ìPAT¬î HACKETT"] <- "PATRICIA PAT HACKETT"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "RA√∫L M. GRIJALVA"] <- "RAúL M. GRIJALVA"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "ROBERT C. ¬ìBOBBY¬î SCOTT"] <- "ROBERT C. BOBBY SCOTT"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "ROBERT G. ¬ìBOB¬î GOOD"] <- "ROBERT G. BOB GOOD"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "SANDRA ¬ìCANDY¬î CHRISTOPHE"] <- "SANDRA CANDY CHRISTOPHE"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "TOM O¬íHALLERAN"] <- "TOM O'HALLERAN"
cand_list[["house"]]$candidate[
  cand_list[["house"]]$candidate == "WILLIAM P. ¬ìBILL¬î OLSON"] <- "WILLIAM P. BILL OLSON"


# Apparently, none of this actually worked.

# Match candidate-level characteristics ========================================

## FB original data
fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>%
      ## Joining, by = "candidate"
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )

assert_that(nrow(fb_list$senate) == nrow(fb_matched$senate))
assert_that(nrow(fb_list$house) == nrow(fb_matched$house))
summary(fb_matched$senate$vote_share)
summary(fb_matched$house$vote_share)
assert_that(
  !any(
    fb_matched$senate$page_id != fb_matched$senate$fb_ad_library_id,
    na.rm = TRUE ## Newly added candidates who didn't run in 2020
  )
  ## Problem with things like 1.58529E+15
)

fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      select(-fb_ad_library_id) %>%
      select(fb_ad_library_id = page_id, everything())
  )

# Organize party variable ======================================================
fb_matched$senate <- fb_matched$senate %>% rename(party = party_simplified)
fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      mutate(
        party = case_when(
          ## DEMOCRATIC-FARMER-LABOR and also DEMOCRATIC-NONPARTISAN LEAGUE
          grepl("DEMOCRAT", party) ~ "DEMOCRAT",
          TRUE ~ party
        )
      ) %>%
      rename(id_old = id) %>%
      rename(id = id_2)
  )

# Filling in missing candidates ================================================

# Senate:
## Rand Paul
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 inc[candidate == 'RAND PAUL'] <- 'INCUMBENT')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 state_name[candidate == 'RAND PAUL'] <- 'KENTUCKY')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 district[candidate == 'RAND PAUL'] <- 'statewide')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 party_detailed[candidate == 'RAND PAUL'] <- 'REPUBLICAN')
## Pat Toomey
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 inc[candidate == 'PAT TOOMEY'] <- 'INCUMBENT')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 state_name[candidate == 'PAT TOOMEY'] <- 'PENNSYLVANIA')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 district[candidate == 'PAT TOOMEY'] <- 'statewide')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 party_detailed[candidate == 'PAT TOOMEY'] <- 'REPUBLICAN')
## Alex Padilla
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 inc[candidate == 'ALEX PADILLA'] <- 'OPEN')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 state_name[candidate == 'ALEX PADILLA'] <- 'CALIFORNIA')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 district[candidate == 'ALEX PADILLA'] <- 'statewide')
fb_matched[["senate"]] <- within(fb_matched[["senate"]], 
                                 party_detailed[candidate == 'ALEX PADILLA'] <- 'DEMOCRAT')

## For our House candidates, it looks like incumbency is the only thing missing
## This will make things simpler!

matched_house_nas <- as.data.frame(cbind(fb_matched[["house"]]$candidate,
                                          fb_matched[["house"]]$inc,
                                          fb_matched[["house"]]$state_name,
                                          fb_matched[["house"]]$district,
                                          fb_matched[["house"]]$party))
matched_house_nas <- unique(matched_house_nas)

# Saving fb_matched ============================================================

save(fb_matched, file = here("data", "tidy", "fb_matched.Rda"))

# Generate and store metadata for ads, before taking out unique ads ============
fb_simple <- fb_matched %>%
  map(
    ~ .x %>%
      group_by(candidate, ad_creative_body) %>%
      select(
        matches(
          ## Excluding NA values for demo/region target
          paste0(
            paste0(
              "unknown|male|impression|potential|spend|ad_creation_date|",
              "delivery|"
            ),
            "^", tolower(state.abb) %>% paste(collapse = "$|^"), "$"
          )
        )
      )
  )
assert_that(length(setdiff(tolower(state.abb), names(fb_simple$senate))) == 0)

## The list(mean, sd), .names = "{.col}.fn{.fn}" approach didn't work;
## Not sure why; but since I also need na.rm = TRUE, just use the agg. approach
fb_meta <- fb_simple %>%
  map(
    ~ {
      out_mean <- .x %>% summarise(
        across(
          everything(), function(x) mean(as.numeric(x), na.rm = TRUE),
          .names = "mean_{.col}"
        )
      )
      out_min <- .x %>% summarise(
        across(
          everything(), function(x) min(as.numeric(x), na.rm = TRUE),
          .names = "min_{.col}"
        )
      )
      out_max <- .x %>% summarise(
        across(
          everything(), function(x) max(as.numeric(x), na.rm = TRUE),
          .names = "max_{.col}"
        )
      )
      out_sd <- .x %>% summarise(
        across(
          everything(), function(x) sd(as.numeric(x), na.rm = TRUE),
          .names = "sd_{.col}"
        )
      )
      return(
        left_join(left_join(out_mean, out_sd), left_join(out_min, out_max))
      )
    }
  )

## Make sure to NA the NaN values
## Make sure to as.Date(x, origin = "1970-01-01") for wrangled dates
save(fb_meta, file = here("data", "tidy", "fb_meta.Rda"))
