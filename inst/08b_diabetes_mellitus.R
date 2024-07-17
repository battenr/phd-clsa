# Title: Regression Analysis for Diabetes Mellitus ----

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(broom)

load("data/analytic_dataset.Rdata")

df = df %>% 
  mutate(
    diabetes_mellitus = ifelse(is.na(diabetes_mellitus) == TRUE, "no", diabetes_mellitus)
  )

#.... Functions ----

source("R/format_data.R")
source("R/univar_analysis.R") # note: this can take a while to load
source("R/check_confounding.R")

#... Outcome ----

outcome = "diabetes_mellitus"

# Data Setup ----

df_regression <- format_data(outcome = "diabetes_mellitus")

#... Survey Design ----

design_analytic <- svydesign(data = df_regression,
                             weights= ~wghts_analytic, 
                             strata = ~geostrata,
                             #fpc = ~strata_total,
                             ids = ~1)

# Stepwise Variable Selection ----

covars <- c("bzd",
            "age", 
            "sex",
            "province",
            #"region", # region of Canada is used instead of province
            "marital_status", 
            "smoke",
            "education", 
            #"household_income",
            "income",
            "urban_rural"
) 

#... Univariate Analysis ----

univar_result <- covars %>% 
  purrr::map_dbl(
    ~univar(outcome = outcome, var = .x)
  ) %>% 
  enframe(name = "variable", value = "p_value") %>% 
  cbind(covars) %>% 
  filter(p_value < 0.20)


#... Multivariable Analysis ----

multivar_mod <- svyglm(
  reformulate(
    response = outcome, 
    termlabels = c(univar_result$covars)
  ),
  design = design_analytic,
  family = binomial(link = "logit")
)

broom::tidy(multivar_mod)



lrt_test <- function(mod, x){
  
  result <- regTermTest(model = mod, 
                        test.terms = x, 
                        method = "LRT")
  
  result$p
}

keep_vars <- univar_result$covars %>% 
  map_dbl(
    ~lrt_test(multivar_mod, .x)
  ) %>% 
  enframe(name = "variable", value = "p_value") %>% 
  cbind(univar_result$covars) %>% 
  filter(p_value < 0.05) %>% 
  rename(
    var = `univar_result$covars`
  )

# Checking if Confounder ----

univar_result %>% 
  filter(
    !(covars %in% keep_vars$var)
  ) %>% select(covars)

# Education was dropped so check if it's a confounder. 

check_confounding(fully_adjusted =   
                    reformulate(response = outcome, 
                                termlabels = c(univar_result$covars)
                    ),
                  partially_adjusted = 
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var, "bzd")
                    )
)

# Need to add smoking back in 





# Check for Interaction Terms ----- 

# For diabetes, thinking of: 
# - bzd*income
# - bzd*marital_status
# - bzd*region



# Checking by looking at by checking across stratum

# BZD*Income

table_matrix <- svytable(~diabetes_mellitus + bzd + income, 
                         design = design_analytic) %>% 
  as.matrix() 

table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])


# BZD * Marital Status

table_matrix <- svytable(~diabetes_mellitus + bzd + marital_status, 
                         design = design_analytic) %>% 
  as.matrix() 


table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])

# include it

# BZD*region

table_matrix <- svytable(~diabetes_mellitus + bzd + province, 
                         design = design_analytic) %>% 
  as.matrix() 


table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])


# Testing Interactions in Model ----

interaction_terms <- c(
  "bzd*province"#,
  #"bzd*income"#,
  # "bzd*marital_status"
)


mod_interaction <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, 
                                       "bzd",
                                       interaction_terms)
  ),
  design = design_analytic, 
  family = binomial(link = "logit")
)

broom::tidy(mod_interaction)

survey::regTermTest(mod_interaction, 
                    "bzd:province",
                    method = "LRT")

# Final Model ----

# No interaction terms 

final_model <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, 
                                       "bzd")
  ),
  design = design_analytic,
  family = stats::binomial(link = "logit")
)

broom::tidy(final_model) %>% 
  filter(grepl("bzd", term)) %>% 
  mutate(
    or_lower = exp(estimate - 1.96*std.error),
    or = exp(estimate),
    or_upper = exp(estimate + 1.96*std.error)
  )

survey::regTermTest(final_model,
                    test.terms = "bzd",
                    method = "LRT")


# Checking Diagnostics ----

#... Components for Diagnostics ----

# Need to capture the covariate patterns based on the variable that are included

final_model$formula

df_regression <- df_regression %>% 
  group_by(
    age, 
    sex, 
    province, 
    marital_status, 
    smoke, 
    education, 
    income, 
    bzd
  ) %>% 
  mutate(
    mj = cur_group_id()
  ) %>% 
  ungroup() 

# covar_pattern <- df_regression %>%
#   select(
#     covars,
#     outcome,
#     mj
#   )



df_diag <- df_regression %>% 
  mutate(
    rsj = rstandard(final_model, type = "pearson"), # Pearson standardized residuals
    dj = residuals(final_model, type = "deviance"), # deviance
    hj = hatvalues(final_model), # leverage 
    pred_prob = predict(final_model, type = "response") #, newdata = covar_pattern)
  ) %>% 
  group_by(mj) %>% 
  slice_head() %>% 
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

# Observation 18 would be the problematic one. Going to check if 
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

#... Survey Design ----

design_analytic_updated <- svydesign(data = df_regression %>% 
                                       filter(!(mj %in% c(18, 333, 402))),
                                     weights= ~wghts_analytic, 
                                     strata = ~geostrata,
                                     #fpc = ~strata_total,
                                     ids = ~1,
                                     nest = TRUE)

svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, "bzd")
  ),
  design = design_analytic_updated,
  family = stats::binomial(link = "logit")
) %>% 
  broom::tidy() %>% 
  filter(grepl("bzd", term))

final_model %>% 
  broom::tidy() %>% 
  filter(grepl("bzd", term))

# Conclusion: the change is so minimal on the parameters it doesn't even matter. 

# Side note: I assume this is due to the large sample size that we have 

# Final Result ----

final_model %>% 
  tidy() %>% 
  filter(grepl("bzd", term))
