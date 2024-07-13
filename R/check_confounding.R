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
