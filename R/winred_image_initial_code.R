source(here::here("R", "utilities.R"))
winred_dat <- loadRData(
  here("data", "raw", "2022", "winred", "winred_text_scraped.Rda")
) %>%
  mutate(
    name_clean = gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(name_clean)))
  )

summary(winred_dat)

## The images we need are linked in the following columns: logo and bgimg
## NOT og_image and twitter_image

##### Explanation of Procedure, and of Trial-and-Error #####

# To download the images, I employed the download.file() function from the utils
# package.
# download.file() takes a series of arguments, three of which were necessary for
# my purposes here. Those three are:
## 'url' - the URL of the file to be downloaded;
## 'destfile' - the place where the File is to be saved, and its name; and
## 'method' 
# download.file() became the key tool within the functions I wrote, which 
# employed for-loops to download each individual URL in a given column
# Ultimately, the functions employed took the following general form:
###image_download_type <- function(dat, image, name) {
###  for (i in 1:nrow(dat)) {
###    filepath[i] <- paste("pathway to directory where the image is 
###                          to be saved",name[i],"_type.jpg", sep="")
###    download.file(url = image[i],
###                  destfile = filepath[i],
###                  method = "curl")
###  }
###}
# In this context, 'dat' is the dataset where the URL is contained, 'image' is
# the column of the dataset with the URL, and 'name' is a vector containing a
# name that can be given to the image file.
# This function takes a dataset with columns containing a URL and a name vector.
# This version, and the versions in the code below, hardcodes a computer-
# specific file path, which ought to be amended.
# The filepath[i] line is necessary to create a line-specific file name for all
# images. If the destfile is a constant, every image will be saved with the same
# name, tanking any efforts to compile in bulk. 
# It should be the case that, with a bit of editing to the filepath[i] line's 
# paste call, it can be structured such that the file can be sent to a given
# location with a call along the lines of
## destfile =  here("winred_images", filepath[i])
# In this circumstance, with here() replacing the need for the sprawling, 
# hardcoded file path, 'filename[i]' would be the better name for such a column.
# I shall use this approach while coding on the Democratic side of things.
# Putting image[i] in quotes in the 'url' line (url = "image[i]",) will
# tank the code - it is essentially saying that the URL is, verbatim, image[i],
# which of course it is not.

##### Logo Downloads #####

logo_dat <- winred_dat %>%
  select(name_clean, logo) %>%
  filter(!is.na(logo) & !(logo == ""))

assert_that(!any(duplicated(logo_dat$name_clean)))
image_download_logo_updated(logo_dat, image = "logo", name = "name_clean")

# These are all the logos downloaded.

##### bgimg Downloads #####

bg_dat <- as.data.frame(cbind(winred_dat$name_clean, winred_dat$bgimg))
names(bg_dat) <- c("name_clean", "bgimg")
bg_dat[bg_dat==""] <- NA
bg_dat_naomit <- na.omit(bg_dat)

image_download_bgimg_updated <- function(dat, image, name) {
  for (i in 1:nrow(dat)) {
    filepath[i] <- paste("C:/Users/15167/Dropbox/campaign_contribution_prompts/
                         winred_images/bgimg_images/",
                         name[i],"_bgimg.jpg", 
                         sep="")
    download.file(url = image[i],
                  destfile = filepath[i],
                  method = "curl")
  }
}

image_download_bgimg_updated(bg_dat_naomit, bg_dat_naomit$bgimg, 
                             bg_dat_naomit$name_clean)

##### Other Notes #####
# Going forward, one of the big things I need to tackle is sorting out how to
# download the images without hardcoding a file path. I think this should be
# a straightforward endeavor to undertake as part of the coding for ActBlue, one
# that can be re-applied to this code so that it can be more broadly useful.
# I'll also do more to better-familiarize myself with the 'here' package's uses,
# and I'll look into the 'styler' package as well.
