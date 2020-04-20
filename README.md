# COVID-19 Data Visualizer
This simple Shinyapp (the raw code file is also here) extracts data from John Hopkins University's github repository (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series) and generates interactive plots regarding COVID-19 pandemic. The base country is set as Bangladesh, other countries can be selected for comparative analysis.

# Run this code to run this app on your own machine
### function for checking if the required packages exist
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  #sapply(pkg, require, character.only = TRUE)
}

### list of required packages
packages <-
  c("readr",
    "plyr",
    "dplyr",
    "plotly",
    "shiny",
    "shinythemes",
    "shinyjs")

### run the function and install if any package is missing
check.packages(packages)

# finally run the app
shiny::runGitHub("covid-19_v2", "tanvird3") 
