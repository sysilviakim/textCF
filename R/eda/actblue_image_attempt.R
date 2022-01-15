source(here::here("R", "utilities.R"))

# Import =======================================================================

actblue_federal_dat <- loadRData(
  here("data", "tidy", "actblue_federal_2022.Rda")
) %>% mutate(
  display_name = gsub(" ", "_", trimws(display_name)),
  display_name = tolower(display_name)
)

# Creating an ID Column from display_name and id_main ==========================

#class(actblue_federal_dat$display_name)
#class(actblue_federal_dat$id_main)
# The id_main is an integer, and needs to be made a character to merge
actblue_federal_dat$id_main <- as.character(actblue_federal_dat$id_main)
main_name_id <- paste(actblue_federal_dat$display_name,
                      actblue_federal_dat$id_main, sep = "_")
main_name_id <- as.data.frame(main_name_id)

subsetting_dat <- cbind(main_name_id, actblue_federal_dat)

# Pulling Image URL columns into dataframes for image download =================

actblue_image <- subset(subsetting_dat, select =  c("main_name_id", 
                                                    "image_url"))
actblue_mobile_image <- subset(subsetting_dat, select =  c("main_name_id", 
                                                           "mobile_image_url"))
actblue_header_image <- subset(subsetting_dat, select =  c("main_name_id", 
                                                           "header_image_url"))
actblue_background_image <-subset(subsetting_dat, 
                                  select =  c("main_name_id", 
                                              "background_image_url_111"))

# Getting rid of NAs and duplicate URLs ========================================

actblue_image <- actblue_image[!is.na(
  actblue_image$image_url),]
actblue_image <- actblue_image[!duplicated(actblue_image$image_url),]
# Down from 27,964 to 689

actblue_mobile_image <- actblue_mobile_image[!is.na(
  actblue_mobile_image$mobile_image_url),]
actblue_mobile_image <- actblue_mobile_image[!duplicated(
  actblue_mobile_image$mobile_image_url),]
# Down to 669

actblue_header_image <- actblue_header_image[!is.na(
  actblue_header_image$header_image_url),]
actblue_header_image <- actblue_header_image[!duplicated(
  actblue_header_image$header_image_url),]
# Down to 494

actblue_background_image <- actblue_background_image[!is.na(
  actblue_background_image$background_image_url_111),]
actblue_background_image <- actblue_background_image[!duplicated(
  actblue_background_image$background_image_url_111),]
# Down to 260

# Changing Column Names for neater filename purposes ===========================

colnames(actblue_image)[2] <- "image"
colnames(actblue_mobile_image)[2] <- "mobile"
colnames(actblue_header_image)[2] <- "header"
colnames(actblue_background_image)[2] <- "bgimg"

# Image Download Code ==========================================================

image_download(dat = actblue_image, 
               image = "image", 
               name = "main_name_id", 
               pathway = "data/raw/2022/actblue/image")

image_download(dat = actblue_mobile_image, 
               image = "mobile", 
               name = "main_name_id", 
               pathway = "data/raw/2022/actblue/mobile")

image_download(dat = actblue_header_image, 
               image = "header", 
               name = "main_name_id", 
               pathway = "data/raw/2022/actblue/header")

image_download(dat = actblue_background_image, 
               image = "bgimg", 
               name = "main_name_id", 
               pathway = "data/raw/2022/actblue/bgimg")



