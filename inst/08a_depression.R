# Title: Regression Analysis for Depression  ----

# Description: Regression analysis for depression ----

# Originally the goal was to try and do this with a large function. I decided 
# (July 12, 2024) that it was better to have it broken down so we can assess it at each step. 

# This means each script is longer, but we can stop at each section to see if it makes sense. 

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)

load("data/analytic_dataset.Rdata")

#.... Functions ----

source("R/format_data.R")
source("R/univar_analysis.R") # note: this can take a while to load
source("R/check_confounding.R")

#... Outcome ----

outcome = "depression"

# Data Setup ----

df_regression <- format_data(outcome = "depression")

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
                                termlabels = c(keep_vars$var, 
                                               "education")
                    ),
                  partially_adjusted = 
                    reformulate(response = outcome, 
                                termlabels = c(keep_vars$var)
                    )
)

# Check for Interaction Terms ----- 

keep_vars$var

# For depression, thinking of: 
# - bzd*sex 
# - bzd*income
# - bzd*marital_status

# Checking by looking at by checking across stratum

# BZD*Sex 

table_matrix <- svytable(~depression + bzd + sex, 
         design = design_analytic) %>% 
  as.matrix() 

table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

# need to include BZD*sex

# BZD * Income 

table_matrix <- svytable(~depression + bzd + income, 
                         design = design_analytic) %>% 
  as.matrix() 


table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])

# include it

# BZD*marital_status 

table_matrix <- svytable(~depression + bzd + marital_status, 
                         design = design_analytic) %>% 
  as.matrix() 


table_matrix[4,1]*table_matrix[1,1]  / (table_matrix[3,1]*table_matrix[2,1])

table_matrix[8,1]*table_matrix[5,1]  / (table_matrix[7,1]*table_matrix[6,1])

table_matrix[12,1]*table_matrix[9,1]  / (table_matrix[11,1]*table_matrix[10,1])


# Testing Interactions in Model ----

interaction_terms <- c(
  "bzd*sex",
  "bzd*income",
  "bzd*marital_status"
)


mod_interaction <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, interaction_terms)
  ),
  design = design_analytic, 
  family = binomial(link = "logit")
)

broom::tidy(mod_interaction)

survey::regTermTest(mod_interaction, 
            "bzd:sex",
            method = "LRT")


regTermTest(mod_interaction, 
            "bzd:income",
            method = "LRT"
            )

regTermTest(mod_interaction, 
            "bzd:marital_status",
            method = "LRT"
)

# Final Model ----

final_model <- svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, "bzd*sex")
  ),
  design = design_analytic,
  family = stats::binomial(link = "logit")
)

broom::tidy(final_model) %>% 
  filter(grepl("bzd", term))


# Checking Diagnostics ----

final_model$formula

#... Components for Diagnostics ----

df_regression <- df_regression %>% 
  group_by(
    bzd, 
    age, 
    sex, 
    province, 
    marital_status, 
    smoke, 
    income
  ) %>% 
  mutate(
    mj = cur_group_id()
  ) %>% 
  ungroup() 

df_diag <- df_regression %>% 
  
  mutate(
    rsj = rstandard(final_model, type = "pearson"), # Pearson standardized residuals
    dj = residuals(final_model, type = "deviance"), # deviance
    hj = hatvalues(final_model), # leverage 
    pred_prob = predict(final_model, type = "response")
  ) %>% 
  group_by(    bzd, 
               age, 
               sex, 
               province, 
               marital_status, 
               smoke, 
               income) %>% 
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

# Observation 367 would be the problematic one. Going to check if 
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
                                       filter(mj != 367),
                             weights= ~wghts_analytic, 
                             strata = ~geostrata,
                             #fpc = ~strata_total,
                             ids = ~1,
                             nest = TRUE)

svyglm(
  formula = reformulate(response = outcome, 
                        termlabels = c(keep_vars$var, "bzd*sex")
  ),
  design = design_analytic_updated,
  family = stats::binomial(link = "logit")
) %>% 
  tidy() %>% 
  filter(grepl("bzd", term))

final_model %>% 
  tidy() %>% 
  filter(grepl("bzd", term))

# Conclusion: the change is so minimal on the parameters it doesn't even matter. 

# Side note: I assume this is due to the large sample size that we have 

# Final Result ----

final_model %>% 
  broom::tidy() %>% 
  filter(grepl("bzd", term))

emmeans(final_model, "bzd*sex")

survey::regTermTest(final_model, "bzd")
survey::regTermTest(final_model, "bzd:sex")

cov(final_model$naive.cov)

as.data.frame(as.table(final_model$naive.cov)) %>% 
  filter(grepl("bzd", Var1) | grepl("sex", Var1), 
         grepl("bzd", Var2) | grepl("sex", Var2)) %>% 
  pivot_wider(
    values_from = Freq,
    names_from = Var2
  ) 


# Calculating SE for the interaction term. It isn't simply the SE of the 
# interaction because we have to add it to the estimate for BZD. So for 
# BZD*sex it's 

# From Hosmer & Lemeshow it's 

# (f1-f0)^2 * Var(Beta1) + [x(f1-f0)]^2*Var(Beta3) + 2*x(f1-fo)^2 * Cov(Beta1, beta3)

# where beta 1 would be BZD and beta 3 is bzd*sex

0.00963 + 0.0255 + 2*1*(-0.00952)

sqrt(0.01609) # this is the SE for the interaction term 







