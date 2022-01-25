renv::init()

# The usual, including Kmisc ===================================================
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Install typically used libraries
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("assertthat")
install.packages("styler")
install.packages("janitor")

# Quanteda related packages ====================================================
## https://quanteda.io/articles/quickstart.html
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages("quanteda.textmodels")
install_github("quanteda/quanteda.corpora")
install_github("kbenoit/quanteda.dictionaries")

# FB Ad Library ================================================================
install_github("facebookresearch/Radlibrary")

renv::snapshot()