source(here::here("R", "utilities.R"))
winred_dat <- 
  loadRData(here("data", "raw", "2022", "winred", "winred_text_scraped.Rda"))

##### Foreword #####

# Most of the code here is identical to code from an earlier R script entitled 
# winred_images, which I am retaining for my records.
# winred_images features 99 lines of code that constitute my initial attempts
# to craft a function to download images in bulk, with my own rather rambling 
# comments interspersed throughout. For the purposes of clarity, in this version 
# of the script, I have replaced that section of trial-and-error with a section 
# detailing the approach I took, why it works, and why previous versions 
# did not work.

##### Loading Data #####

# I used the git clone procedure to get this into a Dropbox folder, so there 
# should not be any way for me to overwrite the data here.
# But just in case, I'm creating a copy of the data - there's no way I can
# overwrite the original data from this. 
# (I think and hope, at least. At times like these, fortune favors the careful.)
summary(winred_dat)
# The images we need are linked in the following columns: logo and bgimg
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

logo_dat <- as.data.frame(cbind(winred_dat$name_clean, winred_dat$logo))
names(logo_dat) <- c("name_clean", "logo")
# There are some cases where there is no Logo url. I don't want to have to deal
# with those - I imagine they'll mess up my code halfway through.
#logo_no_blanks <- logo_dat[!apply(logo_dat$logo == "", 1, all),]
# This wasn't working...but there's a straightforward fix, as all the name_clean
# items are complete.
logo_dat[logo_dat==""] <- NA
logo_dat_naomit <- na.omit(logo_dat)

image_download_logo_updated <- function(dat, image, name) {
  for (i in 1:nrow(dat)) {
    filepath[i] <- paste("C:/Users/15167/Dropbox/campaign_contribution_prompts/
                         winred_images/logo_images/",
                         name[i],"_logo.jpg", 
                         sep="")
    download.file(url = image[i],
                  destfile = filepath[i],
                  method = "curl")
  }
}

image_download_logo_updated(logo_dat_naomit, logo_dat_naomit$logo, 
                            logo_dat_naomit$name_clean)

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
