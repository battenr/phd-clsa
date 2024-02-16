# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(officer)
library(flextable)


#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Data ----

load("data/analytic_dataset.Rdata")



# Steps for Analysis ----

# 1. Univariate analysis 
# 2. Fit model with all that were p<0.20
# 3. Remove those that aren't p < 0.05
# 4. LR test comparing model with the significant terms vs ones that aren't (if p > 0.05 then okay)
# 5. Check if confounder
# [(theta | c) - (theta | no c)] / (theta | c)

# 6. Check plausible interaction terms: 
# 

# 7. Check assumptions

# - Check that errors are independent (similar to MSc)
# - Check leverage and Cook's distance

# Code to use ####

# All covariates: 

df %>% colnames()

#... Univariate Analysis ----

# 





