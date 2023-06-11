# Title: Loading Data 

# Description: Code for loading CLSA data into R 

# Setup ----

#... Libraries ----

library(tidyverse)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Simulated Data ----