# Title: Regression Analysis for Pneumonia  ----

# Description: regression analysis for pneumonia ----

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(broom)

load("data/analytic_dataset.Rdata")

#.... Functions ----

source("R/format_data.R")
source("R/univar_analysis.R") # note: this can take a while to load
source("R/check_confounding.R")

#... Outcome ----

outcome = "pneumonia"

# Data Setup ----

df_regression <- format_data(outcome = "pneumonia")

#... Survey Design ----

design_analytic <- svydesign(data = df_regression,
                             weights= ~wghts_analytic, 
                             strata = ~geostrata,
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
  filter(p_value < 0.05 | `univar_result$covars` == "bzd") %>% 
  rename(
    var = `univar_result$covars`
  )

# Checking if Confounder ----

univar_result %>% 
  filter(
    !(covars %in% keep_vars$var)
  ) %>% select(covars)

# Checking marital_status and smoke 

#... Marital Status ----

check_confounding(fully_adjusted =   
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var,
                                               "age", 
                                               "sex", 
                                               "marital_status")
                    ),
                  partially_adjusted = 
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var,
                                               "age", 
                                               "sex")
          
                    )
)

#... Smoke ----

check_confounding(fully_adjusted =   
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var,
                                               "age", 
                                               "sex", 
                                               "smoke")
                    ),
                  partially_adjusted = 
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var,
                                               "age", 
                                               "sex")
                                
                    )
)

#... What is included ----

# Based on the quantiative approach, none of the removed variables are 
# confounders 

# Check for Interaction Terms ----- 

keep_vars$var # all of the variables that are going to be included 

# For pneumonia, thinking of: 

# - bzd*sex
# - bzd*age
# - bzd*smoke

keep_vars$var

# Checking by looking at by checking across stratum

# BZD*Sex

table_matrix <- svytable(~pneumonia + bzd + sex, 
                         design = design_analytic)  %>% 
  as.matrix() 

table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

# BZD*Smoke

table_matrix <- svytable(~pneumonia + bzd + smoke, 
                         design = design_analytic) %>% 
  as.matrix() 

table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])

# include it

# Testing Interactions in Model ----

# Terms: 

interaction_terms <- c(
  "bzd*sex",
  "bzd*age",
  "bzd*smoke"
  #"bzd*income"#,
  # "bzd*marital_status"
)

mod_interaction <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, 
                                       "age",
                                       "sex",
                                       "marital_status",
                                       "smoke",
                                       interaction_terms)
  ),
  design = design_analytic, 
  family = binomial(link = "logit")
)

broom::tidy(mod_interaction)

survey::regTermTest(mod_interaction, 
                    "bzd:age",
                    method = "LRT")

survey::regTermTest(mod_interaction, 
                    "bzd:sex",
                    method = "LRT")

survey::regTermTest(mod_interaction, 
                    "bzd:smoke",
                    method = "LRT")

# Final Model ----

# No interaction terms 

final_model <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var,
                                       "age",
                                       "sex",
                                       "marital_status", 
                                       "smoke", 
                                       "bzd*smoke")
  ),
  design = design_analytic,
  family = stats::binomial(link = "logit")
)

broom::tidy(final_model) %>% 
  filter(grepl("bzd", term))

final_model$formula

# Checking Diagnostics ----

#... Components for Diagnostics ----

# Need to capture the covariate patterns based on the variable that are included

df_regression <- df_regression %>% 
  group_by(
    bzd, 
    province,
    income,
    age,
    sex,
    marital_status, 
    smoke
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

# Covariate pattern 47 would be the problematic one. Going to check if 
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

# mj 

# Having to Remove Covariate Pattern ----

# If having to remove a covariate pattern, then need to remove it and 
#repeat the process of fitting a model. 

#... Survey Design ----

design_analytic_updated <- svydesign(data = df_regression %>% 
                                       filter(!(mj %in% c(7031, 7153, 7024, 7142))
                                       ),
                                     weights= ~wghts_analytic, 
                                     strata = ~geostrata,
                                     #fpc = ~strata_total,
                                     ids = ~1,
                                     nest = TRUE)
svyglm(
  formula = final_model$formula,
  design = design_analytic_updated,
  family = stats::binomial(link = "logit")
) %>% 
  broom::tidy() %>% 
  filter(grepl("bzd", term))

# Conclusion: the change is so minimal on the parameters it doesn't even matter. 

# Side note: I assume this is due to the large sample size that we have 

# Final Result ----

# If no covariate patterns removed. 

final_model %>%
  tidy() %>%
  filter(grepl("bzd", term)) %>%
  mutate(
    or_lower = exp(estimate - 1.96*std.error),
    or = exp(estimate),
    or_upper = exp(estimate + 1.96*std.error)
  ) %>% view()

survey::regTermTest(final_model, "bzd:smoke", method = "LRT")

final_model$formula

# Calculating SE for the interaction terms 

cov(final_model$naive.cov)

as.data.frame(as.table(final_model$naive.cov)) %>% 
  filter(grepl("bzd", Var1), 
         grepl("bzd", Var2)) %>% 
  pivot_wider(
    values_from = Freq,
    names_from = Var2
  )  %>% view()


# From Hosmer & Lemeshow it's 

# (f1-f0)^2 * Var(Beta1) + [x(f1-f0)]^2*Var(Beta3) + 2*x(f1-fo)^2 * Cov(Beta1, beta3)

# where beta 1 would be BZD and beta 3 is bzd*smoke

# For BZD*Smoke=Yes:

sqrt(0.04793115 + 0.12966334 + 2*1*(-0.04741667)) # 0.288

exp(1.188 + 0.273 + 1.96*0.288)
exp(1.188 + 0.273 - 1.96*0.288)

exp(1.188 + 0.273 + 1.96*0.288)

# For BZD*Smoke = "No"

sqrt(0.04793115 + 0.39928224 + 2*1*(-0.04726679)) # 0.593

exp(1.188 - 2.08 + 1.96*0.593)

exp(1.188 - 2.08 - 1.96*0.593)






