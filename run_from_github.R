# function for checking if the required packages exist
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  #sapply(pkg, require, character.only = TRUE)
}

# list of required packages
packages <-
  c("readr",
    "plyr",
    "dplyr",
    "plotly",
    "shiny",
    "shinythemes",
    "shinyjs")

# run the function and install if any package is missing
check.packages(packages)

# finally run the app
shiny::runGitHub("covid-19_v2", "tanvird3")