# Install PoolTestR: r-universe dev version
# options(repos = c(
#   CRAN = "https://cloud.r-project.org",
#   angusmclure = "https://angusmclure.r-universe.dev"
# ))
# install.packages("PoolTestR")

# Install PoolPoweR: github dev version
# devtools::install_github("AngusMcLure/PoolPoweR")

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

source("R/validation.R")
source("R/tooltips_ui.R")
source("R/design_text.R")
source("R/other_ui.R")
