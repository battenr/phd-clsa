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

# 6. Check plausible interaction terms: (four)

# This is too many interaction terms. I'm going to pick a couple, that are 
# interaction terms with respective to the treatment. So bzd*whatever. Remember
# that this has to do with the outcome. So bzd*sex there may be a difference w/r
# to heart disease (since mostly males are affected)


# - sex*bzd :

age*sex
household_income*education
age*urban_rural
urban_rural*province

# 

# 7. Check assumptions

# - The previous assumptions were wrong. My outcomes are all based on 
# logistic regression. So because of this, I'm going to use the diagnostic plots 
# that are outlined in Hosmer and Lemeshow: 

# - deltaChi vs predicted prob
# - deltaD (deviance) vs predicted prob
# - deltaB (change in beta estimate vs predicted prob)





