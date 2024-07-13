stepwise_selection <- function(outcome, 
                                df_regression = df_regression){
  
  library(tidyverse) # ol faithful 
  library(survey) # used since this is survey data 
  
  # Covariates that we are using
  
  covars <- c("bzd",
              "age", 
              "sex",
              #"province",
              "region", # region of Canada is used instead of province
              "marital_status", 
              "smoke",
              "education", 
              #"household_income",
              "income",
              "urban_rural"
  ) 
  
  # Calculating the number of patients. This is going to be different 
  # for each analysis 
  
  # total sample size remaining
  
  num_remaining <- df_regression %>% 
    dplyr::select(
      covars, 
      outcome,
      wghts_analytic, 
      geostrata) %>%
    drop_na() %>% nrow()
  
  # Number of patients with a BZD 
  
  num_bzd <- df_regression %>% 
    dplyr::select(
      covars, 
      outcome,
      wghts_analytic, 
      geostrata) %>%
    drop_na() %>% count(bzd)
  
  # Creating the survey design objects 
  
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
  
  univar <- function(outcome, 
                     var, 
                     design = design_analytic){
    
    # Reformulating as outcome ~ variable
    
    formula <- reformulate(as.character(paste(outcome, "~", var)))
    
    # Fitting glm 
    
    model <- survey::svyglm(paste(outcome, "~", var), 
                            design = design,
                            family = binomial(link = "logit"))
    
    # Using the working likelihood ratio (Rao-Scott) test
    
    term_test <- regTermTest(model, 
                          test.terms = var, 
                          method = "LRT")
    
    term_test$p
    
  }
  
  # Step 2 #### 
  
  univariate_results <- covars %>%
    
    # Applying the univariate function above to each of the covariates 
    
    purrr::map_dbl(
      ~univar(outcome, var = .x)
    ) %>%
    
    cbind(covars) %>% 
    as.data.frame() %>% 
    
    mutate(
      
    )
  
  univariate_results$p_value <- univariate_results$.
    
    # Selecting only those variables with a p-value less than  0.20 
  
  univariate_results <- univariate_results %>% 
    
    filter(p_value < 0.20) %>%
    
    # Making rownames a column so can extract the variable 
    
    as_tibble(rownames = "term") %>%
    
    # Selecting only the term 
    
    pull(term)
  
  # Step 3 - Fitting Multivariable Model ####
  
  multivariable_formula <- reformulate(
    response = outcome, 
    termlabels = unique(c("bzd", univariate_results))
  )
  
  # Fitting the model 
  
  multivariable_results <- svyglm(
    reformulate(
      response = outcome, 
      termlabels = unique(c("bzd", univariate_results))
    ),
    design = design_analytic,
    family = binomial()
  ) %>%
    car::Anova(type = "III") %>% # using type III sum of squares 
    
    # Choosing only those 
    
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



