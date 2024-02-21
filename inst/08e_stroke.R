# Title: Regression Analysis for Stroke  ----

# Description: regression analysis for stroke ----

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(svydiags)

load("data/analytic_dataset.Rdata")

#.... Functions ----

source("R/stepwise_regression_functions.R") # note: this can take a while to load
source("R/svyglm_checks_prep.R")
source("R/svyglm_checks.R")
source("R/outliers_svy_sa.R")


# Stepwise Regression ----

stepwise_regression(outcome = "stroke")

# Model Diagnositics ----

#... Data Setup 

prep = svyglm_checks_prep("stroke")

checks = svyglm_checks(
  "stroke",
  formula = stroke ~ bzd + age + marital_status + income,
  df = prep$`Data with Outcome and Covars`,
  design = prep$`Survey Design`)

checks$VIF
nrow(checks$`Problematic Outliers`)
checks$`Residuals vs ID`

# Refitting model 

# Refitting model due to VIF. Using centered age instead

checks = svyglm_checks(
  "stroke",
  formula = stroke ~ bzd + centered_age + marital_status + income,
  df = prep$`Data with Outcome and Covars`,
  design = prep$`Survey Design`)

checks$VIF
nrow(checks$`Problematic Outliers`)
checks$`Residuals vs ID`

# Outliers ----

# No outliers so didn't use this section 

#... Viewing Outliers 

# prep$`Data with Outcome and Covars` %>% 
#   dplyr::filter(
#     entity_id %in% checks$`Problematic Outliers`$id
#   ) %>% view()

#... Checking Outliers ----

# Using a sensivitiy analysis to check for outliers

# outliers_sa(mi ~ bzd + centered_age + sex + smoke + income + urban_rural,
#             prep = prep,
#             checks = checks)

# Refitted model ----

# NA 

# Final Model ----

final_model <- svyglm(
  stroke ~ bzd + centered_age + marital_status + income,
  design = prep$`Survey Design`,
  family = stats::binomial(link = "logit")
)

# P-Values ----

car::Anova(final_model, type = "III") %>%
  as_tibble(rownames = "term") %>%
  filter(grepl("bzd", term)) %>%
  mutate(
    `Pr(>Chisq)` = round(`Pr(>Chisq)`, 6)
  ) %>%
  dplyr::select(term, `Pr(>Chisq)`)

# Terms ----

final_model %>%
  broom::tidy() %>%
  filter(grepl("bzd", term)) %>%
  mutate(
    OR = exp(estimate),
    lower_ci = round(exp(estimate - 1.96*std.error),3),
    upper_ci = round(exp(estimate + 1.96*std.error), 3),
    conf_interval_95 = paste(lower_ci, " to ", upper_ci)
  ) %>%
  dplyr::select(
    term, OR, conf_interval_95
  )

final_model$formula
