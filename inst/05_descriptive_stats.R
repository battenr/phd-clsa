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
                                "missing"))
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
  select(
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

svybys(~age, ~bzd, svymean, design = design.infl)

svybys(~age, ~bzd, svyvar, design = design.infl)

sqrt(117.8)



# SD

# To calculate SD you need to calculate variance first then take the sqrt()

sqrt(svyvar(df.na.missing$age, design = design.infl))

# Stratified By BZD ----

# Note: 

# - Totals should add for BZD use. For example, 

# Male/Female and BZD yes/no

# The percentages should equal 100% for the BZD categories (i.e., no should equal 100)

svy_countby(bzd, by = sex, data = df.na.missing) %>% 
  group_by(sex) %>% 
  arrange(sex, bzd)

svy_countby(bzd, by = province, data = df.na.missing) %>% 
  group_by(province) %>% 
  arrange(province, bzd)


svy_countby(bzd, by = marital_status, data = df.na.missing) %>% 
  group_by(marital_status) %>% 
  arrange(marital_status, bzd)


svy_countby(bzd, by = smoke, data = df.na.missing) %>% 
  group_by(smoke) %>% 
  arrange(smoke, bzd)      


svy_countby(bzd, by = education, data = df.na.missing) %>% 
  group_by(education) %>% 
  arrange(education, bzd)


svy_countby(bzd, by = household_income, data = df.na.missing) %>% 
  group_by(household_income) %>% 
  arrange(household_income, bzd)

svy_countby(bzd, by = urban_rural, data = df.na.missing) %>% 
  # group_by(urban_rural) %>% 
  arrange(urban_rural)





svy_countby(bzd, by = anxiety, data = df.na.missing) %>% arrange(anxiety)

svy_countby(bzd, by = depression, data = df.na.missing) %>% arrange(depression)

svy_countby(bzd, by = diabetes_mellitus, data = df.na.missing) %>% arrange(diabetes_mellitus)

svy_countby(bzd, by = hbp, data = df.na.missing) %>%  arrange(hbp)

svy_countby(bzd, by = mi, data = df.na.missing) %>% arrange(mi)

svy_countby(bzd, by = stroke, data = df.na.missing) %>% arrange(stroke) 

svy_countby(bzd, by = cancer, data = df.na.missing) %>% arrange(cancer) 

svy_countby(bzd, by = heart_disease, data = df.na.missing) %>% arrange(heart_disease)

svy_countby(bzd, by = copd, data = df.na.missing) %>% arrange(copd) 

svy_countby(bzd, by = dementia, data = df.na.missing) %>% arrange(dementia)

svy_countby(bzd, by = pneumonia, data = df.na.missing) %>% arrange(pneumonia)



