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

# Formatting Data ----

# Converting all NAs to missing for reporting. This will still be NA in the 
# regression analyses because then they will be dropped

df.na.missing = df %>% mutate_all(~replace_na(., "missing")) %>% 
  mutate(
    household_income = factor(household_income, 
                              levels = c(
                                "<20k",
                                "20k to <50k",
                                "50k to <100k",
                                "100k to <150k",
                                "150k+", 
                                "missing")),
    region = dplyr::case_when(
      province == "Alberta" ~ "Prarie",
      province == "British Columbia" ~ "West",
      province == "Manitoba" ~ "Prarie",
      province == "Newfoundland and Labrador" ~ "Atlantic",
      province == "Nova Scotia" ~ "Atlantic",
      province == "Ontario" ~ "Central",
      province == "Quebec" ~ "Central",
    ),
    income = dplyr::case_when(
      household_income == "<20k" ~ "<20k to <50k", 
      household_income == "20k to <50k" ~ "<20k to <50k",
      household_income == "50k to <100k" ~ "50k to <150k",
      household_income == "100k to <150k" ~ "50k to <150k",
      household_income == "150k+" ~ "150k+",
      household_income == "missing" ~ "missing"
      
    )
  )



unique(df.na.missing$household_income)

# Survey Design ----

design.infl <- svydesign(data = df.na.missing, 
                         weights= ~wghts_inflation, 
                         strata = ~geostrata,
                         #fpc = ~strata_total,
                         ids = ~1,
                         nest = TRUE)

# Overall Cohort ----

#... Character Variables ----

df.na.missing |> 
  dplyr::select(
    -c(entity_id, age, 
       contains("wghts"), 
       geostrata)
  ) |> 
  names() |> 
  map(
    \(x)svy_count(x, design = design.infl, data = df.na.missing)
  ) 

df |> colnames()

#... Continuous Variables ----

# Age 

svymean(df.na.missing$age, design = design.infl) 

# SD

# To calculate SD you need to calculate variance first then take the sqrt()

sqrt(svyvar(df.na.missing$age, design = design.infl))

# Stratified By BZD ----

# Note: 

# - Totals should add for BZD use. For example, 

# Male/Female and BZD yes/no

# The percentages should equal 100% for the BZD categories (i.e., no should equal 100)

# Note to self: figure out how to make a function 

source("R/svy_countby.R")

c("sex", 
  "region",
  "marital_status",
  "smoke",
  "education",
  "income",
  "urban_rural"
  ) %>%
  purrr::map(
    ~svy_countby(.x, byvar = "bzd", data = df.na.missing) %>% arrange(bzd)
  )
   


#... Outcomes ----

c("anxiety", 
  "depression",
  "diabetes_mellitus",
  "hbp",
  "mi", 
  "stroke",
  "cancer",
  "heart_disease",
  "copd",
  "dementia",
  "pneumonia"
) %>%
  purrr::map(
    ~svy_countby(.x, byvar = "bzd", data = df.na.missing) %>% arrange(bzd)
  )

