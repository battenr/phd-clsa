# Title: Descriptive statistics

# Description: Descriptive statistics for the following: 
# - Overall
# - Covariates
# - Stratified by BZD use 
# - Outcomes

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

# Survey Design ----

design.infl <- svydesign(data = df, 
                         weights= ~wghts_inflation, 
                         strata = ~geostrata,
                         #fpc = ~strata_total,
                         ids = ~1,
                         nest = TRUE)

# Overall Cohort ----

#... Character Variables ----

df |> 
  select(
    -c(entity_id, age, 
       contains("wghts"), 
       geostrata)
  ) |> names() |> 
  map(
    \(x)svy_count(x, design = design.infl, data = df)
  ) 

df |> colnames()



#... Continuous Variables ----

# Age 

svymean(df$age, design = design.infl) 

# SD

# To calculate SD you need to calculate variance first then take the sqrt()

sqrt(svyvar(df$age, design = design.infl))

# Stratified By BZD ----

df |> 
  select(
    -c(entity_id, age)
  ) |> colnames() |> 
  map(
    \(x)svy_countby(x, byvar = "bzd", design = design.infl, data = df)
  ) 

df |> 
  select(sex) |> 
  names() |> 
  map(
    \(x)svy_countby(var = x, byvar = "bzd", design = design.infl, data = df)
  ) 


  

df |> 
  select(
    -c(entity_id, age, 
       contains("wghts"), 
       geostrata)
  ) |> names() |> 
  map(
    \(x)svy_count(x, design = design.infl, data = df)
  ) 

svy_countby(sex)

