# Title: Loading Data 

# Description: Code for loading CLSA data into R 

# Setup ----

#... Libraries ----

library(tidyverse)
library(googlesheets4)
library(janitor)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Data ----

dat <- read_csv(file = "data/2206009_MemorialUniversity_GMugford_Baseline_CoPv7.csv") %>% 
  janitor::clean_names()
