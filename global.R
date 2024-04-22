# Install PoolTestR: r-universe dev version
# options(repos = c(
#   CRAN = "https://cloud.r-project.org",
#   angusmclure = "https://angusmclure.r-universe.dev"
# ))
# install.packages("PoolTestR")

# Install PoolPoweR: github dev version
# devtools::install_github("AngusMcLure/PoolPoweR")
appVersion <- "0.1.2"

library(shiny)
library(shinyBS)
library(sortable)
library(DT)
library(dplyr)
library(readxl)
# library(devtools)
library(PoolTestR)
library(PoolPoweR)
library(shinybusy)

source("R/design_text.R")
source("R/other_ui.R")
source("R/tooltips_ui.R")
source("R/validation.R")
source("R/utils_dt_display.R")
source("R/utils_pooltestr.R")
