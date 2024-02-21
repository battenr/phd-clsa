# Title: Regression Analysis for High Blood Pressure ----

# Description: regression analysis for high blood pressure ----


# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(svydiags)

load("data/analytic_dataset.Rdata")

#.... Functions ----

source("R/stepwise_regression_functions.R") # note: this can take a while to load

# Stepwise Regression ----

stepwise_regression(outcome = "hbp")

# Model Diagnostics ----

outcome = "hbp"

# For this section, take necessary code from the svyglm_checks.R code

#... Setup ----

df_regression <- df %>% 
  mutate(centered_age = age - 58) %>% # subtracting mean, calculated using svymean, from age 
  
  # Converting the values of "yes" and "no" to 1 and 0 so 
  # they can be used in the logistic regression
  
  mutate(
    across(
      c("anxiety", "depression", "diabetes_mellitus", "hbp",
        "mi", "stroke", "cancer", "heart_disease", "copd",
        "dementia", "pneumonia"),
      ~case_match(., "yes" ~ 1, "no" ~ 0)
    )
  ) %>% 
  
  dplyr::mutate(
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
      household_income == "150k+" ~ "150k+"
      
    )
  ) %>%
  
  dplyr::select(
    bzd,
    age, 
    centered_age,
    sex,
    province,
    region,
    marital_status,
    smoke,
    education,
    income,
    household_income,
    urban_rural,
    
    # Outcome
    
    outcome,
    
    # 
    
    entity_id,
    
    wghts_analytic, 
    geostrata
    
  ) %>%
  
  # Dropping NA 
  
  drop_na()

# Survey Design ----

design_analytic <- svydesign(data = df_regression,
                             weights= ~wghts_analytic, 
                             strata = ~geostrata,
                             #fpc = ~strata_total,
                             ids = ~1,
                             nest = TRUE)

#... Fitting Model ----

# Model is fit based on output from stepwise regression function 

formula = as.formula("hbp ~ bzd + age + sex + smoke + education + income + age*sex")

mod <- svyglm(
  formula,
  design = design_analytic,
  family = stats::binomial(link = "logit")
)

#... Plotting residuals vs participant id (entity_id) ----

residuals = data.frame(
  id = df_regression$entity_id,
  resid = svydiags::svystdres(mod),
  age = df_regression$age,
  bzd = df_regression$bzd,
  sex = df_regression$sex,
  region = df_regression$region,
  marital_status = df_regression$marital_status,
  smoke = df_regression$smoke,
  education = df_regression$education,
  income = df_regression$income,
  urban_rural = df_regression$urban_rural,
  centered_age = df_regression$age
)

# Starting with Plot of Residuals vs Patient IDs. Can add others as needed
# (i.e., age or centered age, etc)

ggplot(data = residuals, 
       mapping = aes(x = id, y = resid.stdresids)) +
  geom_point()

#... VIF ----

# Checking if any > 10

X1 <- df_regression[order(df_regression$geostrata),]

X2 <- model.matrix(
  ~bzd + age + sex + smoke + education + income + age*sex,
  data = data.frame(X1)
)

vif <- svydiags::svyvif(
  mobj = mod, 
  X = X2[,-1],
  w = df_regression$wghts_analytic,
  stvar = "geostrata",
  clvar = NULL
)

#... Cook's distance ----

df.outliers = data.frame(
  id = df_regression$entity_id,
  cooksd = svyCooksD(
    mobj = mod, 
    stvar = "geostrata"
  ),
  leverage = svydiags::svyhat(mod),
  resid = svydiags::svystdres(mod),
  age = df_regression$age,
  bzd = df_regression$bzd,
  sex = df_regression$sex,
  region = df_regression$region,
  marital_status = df_regression$marital_status,
  smoke = df_regression$smoke,
  education = df_regression$education,
  household_income = df_regression$household_income,
  urban_rural = df_regression$urban_rural,
  centered_age = df_regression$age
)

# Plotting outliers 

ggplot(df.outliers, 
       aes(x = leverage, y = resid.stdresids)) +
  geom_point(aes(size = cooksd), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_size_continuous(range = c(1, 10), guide = FALSE) + # Adjust size range as needed
  labs(x = "Leverage", y = "Standardized Residuals",
       title = "Leverage vs. Standardized Residuals",
       subtitle = "Point size represents Cook's Distance") +
  theme_minimal()

problematic.outliers <- df.outliers %>%
  filter(
    cooksd > 3
  )

#... Checking Outliers ----

df_without_outliers <- df_regression %>%
  dplyr::filter(
    !(entity_id %in% problematic.outliers$id)
  )

design_without_outliers_analytic <- svydesign(data = df_without_outliers,
                                              weights= ~wghts_analytic, 
                                              strata = ~geostrata,
                                              #fpc = ~strata_total,
                                              ids = ~1,
                                              nest = TRUE)

mod_without_outliers <- svyglm(
  depression ~ bzd + centered_age + sex + region + marital_status + smoke + household_income,
  design = design_analytic,
  family = stats::quasibinomial(link = "logit")
)

# Initial model (same as above).

# Comparing estimate and SEs 

mod

broom::tidy(mod_without_outliers)

broom::tidy(mod)

# Refitted model ----

# Check whatever issues there was. In this case, the main issue was VIF. So
# check VIF and overall residuals 

mod_refit <- svyglm(
  hbp ~ bzd + centered_age + sex + smoke + education + household_income + centered_age*sex,
  design = design_analytic,
  family = stats::binomial(link = "logit")
)

#... Plotting residuals vs participant id (entity_id) ----

refitted_residuals = data.frame(
  id = df_regression$entity_id,
  resid = svydiags::svystdres(mod_refit)
)

ggplot(data = refitted_residuals, 
       mapping = aes(x = id, y = resid.stdresids)) +
  geom_point()

#... VIF ----

X1 <- df_regression[order(df_regression$geostrata),]

X2 <- model.matrix(
  ~bzd + centered_age + sex + smoke + education + income + centered_age*sex,
  data = data.frame(X1)
)

vif <- svydiags::svyvif(
  mobj = mod_refit, 
  X = X2[,-1],
  w = df_regression$wghts_analytic,
  stvar = "geostrata",
  clvar = NULL
)

broom::tidy(mod_refit)


# Final Model ----

mod_refit$formula

final_model <- svyglm(
  hbp ~ bzd + centered_age + sex + smoke + education + household_income +
    centered_age*sex,
  design = design_analytic,
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
