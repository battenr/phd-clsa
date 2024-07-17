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
    income,
    #income,
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
           income) %>% 
  # Adding group ID 
  mutate(
    mj = cur_group_id()
  ) %>% 
  
  ungroup() #%>% 
  
  
# filter(
  #   !(mj %in% c(462, 303))
  # )

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

broom::tidy(mod)

1.46 (0.0955) # barely changes the result. 

# Regression Diagnostics ----

#... Predicted Probability ----

test = predict(mod, type = "response") %>% as.data.frame()

#... Components for Diagnostics ----

df_diag <- df_regression %>% 
  mutate(
    rsj = rstandard(mod, type = "pearson"), # Pearson standardized residuals
    dj = residuals(mod, type = "deviance"), # deviance
    hj = hatvalues(mod), # leverage 
    pred_prob = predict(mod, type = "response", newdata = covar_pattern)
  ) %>% 
  mutate(
    deltaChisq = rsj^2,
    deltaB = (rsj*hj) / (1-hj), 
    deltaD = dj^2 / (1-hj)
  )

ggplot(data = df_diag, aes(x = pred_prob, y = deltaD)) + 
  geom_point() + 
  labs(title = "Change in Deviance vs Predicted Probability", 
       x = "\u03C0",
       y = "\u0394D") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) 

# Observation 462 would be the problematic one. Going to check if 
# the parameter estimates really change after removing this observation. 

ggplot(data = df_diag, aes(x = pred_prob, y = deltaChisq)) + 
  geom_point() +
  labs(title = "Change in \u03C7\u00B2 vs Predicted Probability", 
       x = "\u03C0",
       y = "\u03C7\u00B2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) 

ggplot(data = df_diag, aes(x = pred_prob, y = deltaB)) + 
  geom_point() +
  labs(title = "Change in \u0394\u03B2 vs Predicted Probability", 
       x = "\u03C0",
       y = "\u0394\u03B2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) 

# mj 303 

# Having to Remove Covariate Pattern ----

# If having to remove a covariate pattern, then need to remove it and 
#repeat the process of fitting a model. 


# Checking for interaction terms ----

stepwise_regression("depression", df_regression)




