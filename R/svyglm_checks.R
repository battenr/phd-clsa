svyglm_checks <- function(outcome,
                          formula,
                          df,
                          design){
  
  library(tidyverse)
  library(survey)
  library(svydiags)
  
  # Formula ----
  
  # This is the formula
  
  # mod_formula = as.formula(formula)
  
  df = prep$`Data with Outcome and Covars`
  design = prep$`Survey Design`
  
  # Fitting Model ----
  
  mod <- svyglm(
    formula = formula,
    design = design,
    family = stats::binomial(link = "logit")
  )
  
  # Model is fit based on output from stepwise regression function 
  
  residuals = df %>% 
    dplyr::select(
      entity_id
    ) %>% 
    dplyr::mutate(
      resid = svydiags::svystdres(mod)$stdresids
    )
  
  #... Plotting residuals vs participant id (entity_id) ----
  
  plot_residuals_vs_id <- ggplot(data = residuals, 
         mapping = aes(x = entity_id, y = resid)) +
    geom_point()
  
  # VIF ----
  
  X1 <- df[order(df$geostrata),]
  
  formula_char = as.character(formula)
  
  vif_formula = formula(paste(formula_char[-2], collapse = " "))
  
  X2 <- model.matrix(
    formula,
    data = data.frame(X1)
  )
  
  vif <- svydiags::svyvif(
    mobj = mod, 
    X = X2[,-1],
    w = df$wghts_analytic,
    stvar = "geostrata",
    clvar = NULL
  ) %>%
    dplyr::select(
      svy.vif
    ) 
  
  # Cook's Distance ----
  
  df.outliers = data.frame(
    id = df$entity_id,
    cooksd = svyCooksD(
      mobj = mod, 
      stvar = "geostrata"
    )
  )
  
  # Plotting outliers 
  
  problematic.outliers <- df.outliers %>%
    filter(
      cooksd > 3
    ) %>%
    dplyr::select(
      id, 
      cooksd
    )
  
  # Output ----
  
  return(
    list(
      'Residuals vs ID' = plot_residuals_vs_id,
      'VIF' = vif,
      'Problematic Outliers' = problematic.outliers
    )
  )
  
}



