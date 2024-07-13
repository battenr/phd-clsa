# Title: Testing Model Diagnostics ----

# Description: testing model diagnostics for the model. The point of this code is 
# to provide a framework for checking model diagnostics for the regression 

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)

#... Loading Data ----

load("data/analytic_dataset.Rdata")

# Formatting Data ----

# From Hosmer & Lemeshow (for logistic regression) ----

# deltaB = rsj^2 * hj / (1 - hj)
# deltaChiSq = rsj^2
# deltaD = dj^2 / (1-hj)

# we are taking each of these for each covariate pattern


df_regression <- df %>% 
  mutate(centered_age = age - 58) %>% # subtracting mean from age 
  
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
    #income,
    household_income,
    urban_rural,
    
    # Depression
    
    depression,
    
    # 
    
    entity_id,
    
    wghts_analytic, 
    geostrata
    
  ) %>%
  
  # Dropping NA 
  
  drop_na() %>% 
  
  # Grouping to create covariate patterns 
  
  group_by(bzd, 
           age, 
           sex, 
           region, 
           marital_status, 
           smoke, 
           household_income) %>% 
  # Adding group ID 
  mutate(
    mj = cur_group_id()
  ) %>% 
  
  ungroup()

# Survey Design ----

design_analytic <- svydesign(data = df_regression,
                             weights= ~wghts_analytic, 
                             strata = ~geostrata,
                             #fpc = ~strata_total,
                             ids = ~1,
                             nest = TRUE)

# Fitting Model ----

# Model is fit based on output from stepwise regression function 

mod <- svyglm(
  depression ~ bzd + age + sex + region + marital_status + smoke + household_income,
  design = design_analytic,
  family = stats::quasibinomial(link = "logit")
)

# Regression Diagnositcs ----

#... Predicted Probability ----

test = predict(mod, type = "response") %>% as.data.frame()

#... Components for Diagnostics ----

covar_pattern <- df_regression %>% 
  mutate(
    rsj = rstandard(mod, type = "pearson"), # Pearson standardized residuals
    dj = residuals(mod, type = "deviance"), # deviance
    hj = hatvalues(mod) # leverage 
  ) %>% 
  # Grouping by variables in the model for covariate patterns 
  group_by(bzd, 
           age, 
           sex, 
           region, 
           marital_status, 
           smoke, 
           household_income) %>% 
  # Adding group ID 
  mutate(
    mj = cur_group_id()
  ) %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(
    bzd, 
    age, 
    sex, 
    region, 
    marital_status, 
    smoke, 
    household_income,
    rsj,
    dj,
    hj,
    mj
  ) 


predict(mod, type = "response", newdata = covar_pattern)

df_diag <- covar_pattern %>% 
  mutate(
    pred_prob = predict(mod, type = "response", newdata = covar_pattern)
  ) %>% 
  mutate(
    deltaChisq = rsj^2,
    deltaB = (rsj*hj) / (1-hj), 
    deltaD = dj^2 / (1-hj)
  )

ggplot(data = df_diag, aes(x = pred_prob, y = deltaD)) + 
  geom_point()

ggplot(data = df_diag, aes(x = pred_prob, y = deltaChisq)) + 
  geom_point()

ggplot(data = df_diag, aes(x = pred_prob, y = deltaB)) + 
  geom_point()


# deltaB = rsj^2 * hj / (1 - hj)
# deltaChiSq = rsj^2
# deltaD = dj^2 / (1-hj)


# Having to Remove Covariate Pattern ----

# If having to remove a covariate pattern, then need to remove it and repeat the process of fitting a model. 






# Starting with Plot of Residuals vs Patient IDs. Can add others as needed
# (i.e., age or centered age, etc)

ggplot(data = residuals, 
       mapping = aes(x = id, y = resid.stdresids)) +
  geom_point()

# Variance Inflation Factor ----

# Checking for multicollinearity using variance inflation factor (VIF)

# Using 10 as the cutoff

# Setting up the code that is in the help for the svydiags package

X1 <- df_regression[order(df_regression$geostrata),]

X2 <- model.matrix(
  ~bzd + centered_age + sex + region + marital_status + smoke + household_income,
  data = data.frame(X1)
)

vif <- svydiags::svyvif(
  mobj = mod, 
  X = X2[,-1],
  w = df_regression$wghts_analytic,
  stvar = "geostrata",
  clvar = NULL
)

# Cook's distance ----

# Calculating Cook's distance using code from the svydiags package

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

# Outliers ----

# Inspecting outliers to see if the values make sense, etc. 

df_regression %>%
  filter(
    entity_id %in% problematic.outliers$id
  ) %>% view()

#... Sensivitiy Analysis ----

# Modelling with and without outliers 

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

# Initial model (same as above)

mod <- svyglm(
  depression ~ bzd + centered_age + sex + region + marital_status + smoke + household_income,
  design = design_analytic,
  family = stats::quasibinomial(link = "logit")
)

broom::tidy(mod_without_outliers)

broom::tidy(mod)

#..... literally doesn't change it lol

# So going to go ahead and use just svyglm() with no bootstrapping. 





