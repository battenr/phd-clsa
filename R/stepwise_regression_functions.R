# Setup

library(tidyverse)
library(survey)
library(svydiags)


load("data/analytic_dataset.Rdata")

df.regression <- df %>%
  
  # Recoding everything so that it can be 1 and 0 for yes/no for each of the outcomes
  
  dplyr::mutate(
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
)




# outcome = "depression"
# df_regression = df.regression

# Function -----

stepwise_regression <- function(outcome, df_regression = df.regression){
  
  library(tidyverse)
  library(survey)
  
  # Covariates 
  
  covars <- c("bzd",
              "age", 
              "sex",
              #"province",
              "region",
              "marital_status",
              "smoke",
              "education",
              #"household_income",
              "income",
              "urban_rural"
  ) 
  
  # Dataframe for analyses, converting the values to 0 and 1 for regression analyses
  
  # Survey Design Object
  

  
  num_remaining <- df_regression %>% 
    dplyr::select(
      covars, 
      outcome,
      wghts_analytic, 
      geostrata) %>%
    drop_na() %>% nrow()
  
  num_bzd <- df_regression %>% 
    dplyr::select(
      covars, 
      outcome,
      wghts_analytic, 
      geostrata) %>%
    drop_na() %>% count(bzd)
  
  design_analytic <- svydesign(data = df_regression %>% 
                                 dplyr::select(
                                   covars, 
                                   outcome,
                                   wghts_analytic, 
                                   geostrata) %>%
                                 drop_na(),
                               weights= ~wghts_analytic, 
                               strata = ~geostrata,
                               #fpc = ~strata_total,
                               ids = ~1,
                               nest = TRUE)
  
  # Step 1 - Conducting univariate analysis ####
  
  #... Function
  
  univar <- function(outcome, var, design = design_analytic){
    
    formula <- reformulate(as.character(paste(outcome, "~", var)))
    
    model <- survey::svyglm(paste(outcome, "~", var), design = design)
    
    car::Anova(model, type = "III") %>% slice_tail() 
    
  }
  
  # Step 2 #### 
  
  univariate_results <- covars %>%
    
    # Applying 
    
    purrr::map_df(
      ~univar(outcome, var = .x)
    ) %>%
    
    # Selecting only those variables with a p-value less than  0.20 
    
    filter(`Pr(>Chisq)` < 0.20) %>%
    
    # Making rownames a column so can extract the variable 
    
    as_tibble(rownames = "term") %>%
    
    # Selecting only the term 
    
    pull(term)
  
  # Step 3 ####
  
  multivariable_formula <- reformulate(
    response = outcome, 
    termlabels = unique(c("bzd", univariate_results))
  )
  
  multivariable_results <- svyglm(
    reformulate(
      response = outcome, 
      termlabels = unique(c("bzd", univariate_results))
    ),
    design = design_analytic,
    family = binomial()
  ) %>%
    car::Anova(type = "III") %>%
    filter(`Pr(>Chisq)` < 0.05) %>%
    
    # Making rownames a column so can extract the variable 
    
    as_tibble(rownames = "term") %>%
    
    filter(term != "(Intercept)") %>%
    
    # Selecting only the term 
    
    pull(term) 
  
  multivariable_results <- unique(c("bzd", multivariable_results))
  
  removed_vars = symdiff(unique(c("bzd", multivariable_results)), 
                         unique(c("bzd", univariate_results)))
  
  multivariable_mod <- svyglm(
    reformulate(
      response = outcome, 
      termlabels = multivariable_results
    ),
    #multivariable_formula,
    design = design_analytic,
    family = binomial()
  )
  
  # Step 4 ####
  
  
  lrt <- function(var){
    
    formula_with <- reformulate(
      as.character(paste(outcome, "~", paste(multivariable_results, 
        collapse = " + "), " + ", var))
    )
    
    model_with <- svyglm(
      reformulate(
        response = outcome,
        termlabels = c(multivariable_results, var)
      ),
      design = design_analytic)
    
    lr_test <- anova(multivariable_mod, model_with)
    
    lr_test$p < 0.05
    
  }
  
  lrt_results = removed_vars %>%
    map(
      ~lrt(.x)
    ) %>% as.data.frame()
  
# Step 5 - Checking Confounding
  
  check_confounding <- function(fully_adjusted, 
                                partially_adjusted, 
                                design = design_analytic, 
                                family = stats::binomial()){
    
    library(tidyverse)
    library(survey)
    
    mod <- survey::svyglm(
      formula = stats::as.formula(fully_adjusted),
      design = design,
      family = family
    )
    
    mod.updated <- survey::svyglm(
      formula = stats::as.formula(partially_adjusted), 
      design = design,
      family = family
    )
    
    fully.adjusted <- broom::tidy(mod) %>% 
      dplyr::rename(
        estimate_fa = estimate
      ) %>% 
      dplyr::select(
        term,
        estimate_fa
      )
    
    partially.adjusted <- broom::tidy(mod.updated) %>% 
      dplyr::rename(
        estimate_pa = estimate
      ) %>% 
      dplyr::select(
        term,
        estimate_pa
      )
    
    check.confounding <- partially.adjusted %>% 
      dplyr::full_join(
        fully.adjusted,
        by = "term"
      ) %>% 
      dplyr::mutate(
        
        # Checking for if it changes by 20%, don't care about sign
        
        change = (((estimate_fa - estimate_pa) / estimate_fa)*100)
      )
    
    return(check.confounding)
  }
  
  if(length(removed_vars) == 0){
    change_in_coeff <- print("No variables were removed")
  } else{
  
  change_in_coeff <- purrr::map_df(removed_vars, function(removed_var) {
    fully_adjusted_formula <- paste(outcome, "~", paste(multivariable_results, collapse = "+"), "+", removed_var)
    partially_adjusted_formula <- paste(outcome, "~", paste(multivariable_results, collapse = "+"))
    
    check_confounding(fully_adjusted_formula, partially_adjusted_formula)
  }) %>%
    filter(change > 20)
  }
  
  # If there is a change then comeback to this and add an if statement (See chatgpt)
  
  # Interaction Terms ----
  
  interaction_terms <- c("age*sex", 
                         "bzd*sex",
                         "income*education", 
                         "bzd*age", 
                         "urban_rural*region")
  
  
  univariate_interaction_results <- interaction_terms %>%
    
    # Applying 
    
    purrr::map_df(
      ~univar(outcome, var = .x)
    ) %>%
    
    # Selecting only those variables with a p-value less than  0.20 
    
    filter(`Pr(>Chisq)` < 0.20) %>%
    
    # Making rownames a column so can extract the variable 
    
    as_tibble(rownames = "term") %>%
    
    # Selecting only the term 
    
    pull(term) 
  
  univariate_interaction_results <- gsub(":", "*", univariate_interaction_results)
  
  # Multivariable Interaction
  
  multivariable_interaction_formula <- reformulate(
    response = outcome, 
    termlabels = c(multivariable_results, univariate_interaction_results)
  )
  
  multivariable_interaction_results <- svyglm(
    reformulate(
      response = outcome, 
      termlabels = c(multivariable_results, univariate_interaction_results)
    ),
    design = design_analytic,
    family = binomial()
  ) %>%
    car::Anova(type = "III") %>%
    filter(`Pr(>Chisq)` < 0.05) %>%
    
    # Making rownames a column so can extract the variable 
    
    as_tibble(rownames = "term") %>%
    
    filter(term != "(Intercept)") %>%
    
    # Selecting only the term 
    
    pull(term) 
  
  multivariable_interaction_results <- gsub(":", "*", multivariable_interaction_results)
  
  final_formula <- as.character(
    paste(
      outcome, 
      "~", 
      paste(unique(c("bzd", multivariable_interaction_results)), collapse = " + ")
    )
  )
  
  # Fitting Final Model 
  
  return(
    list(
      'Survey Design Object' = design_analytic,
      'Number of Particpants' = num_remaining,
      'Number of BZD Users' = num_bzd,
      'Univariate Results' = univariate_results, 
      'Multivariable Results' = multivariable_results,
      #'Variable '
      'Likelihood Ratio Test' = lrt_results,
      'Change in Coeff' = change_in_coeff,
      'Final Model Terms' = final_formula#,
      # 'Model Terms' = mod_terms,
      # 'P-values (Type III)' = mod_final_p_values
  )
  )
  
  
}



