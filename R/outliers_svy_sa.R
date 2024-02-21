#' Title
#'
#' @param formula 
#' @param prep 
#' @param checks 
#'
#' @return
#' @export
#'
#' @examples
outliers_sa <- function(formula = formula,
                        prep = prep,
                        checks = checks){
  library(tidyverse)
  library(survey)
  library(broom)
  
  df_without_outliers <- prep$`Data with Outcome and Covars` %>%
    dplyr::filter(
      !(entity_id %in% checks$`Problematic Outliers`$id)
    )
  
  design_without_outliers_analytic <- svydesign(data = df_without_outliers,
                                                weights= ~wghts_analytic, 
                                                strata = ~geostrata,
                                                #fpc = ~strata_total,
                                                ids = ~1,
                                                nest = TRUE)
  
  mod_without_outliers <- svyglm(
    formula,
    design = design_without_outliers_analytic,
    family = stats::quasibinomial(link = "logit")
  )
  
  output_without_outliers <- broom::tidy(mod_without_outliers)
  
  # With Outliers 
  
  mod_with_outliers <- svyglm(
    formula,
    design = prep$`Survey Design`,
    family = stats::quasibinomial(link = "logit")
  )
  
  output_with_outliers <- broom::tidy(mod_with_outliers)
  
  return(
    list(
      `Model with No Outliers` = output_without_outliers,
      `Model with Outliers` = output_with_outliers
    )
  )
  
}