#' Univariate Analyses
#'
#' @param outcome outcome variable
#' @param var independent variable
#' @param design survey design object
#'
#' @return Type III p-value 
#' @export
#'
#' @examples
univar <- function(outcome, var, design = design_analytic){
  
  # Making a formula
  
  formula <- reformulate(as.character(paste(outcome, "~", var)))
  
  # Fitting a univariate model 
  
  model <- survey::svyglm(paste(outcome, "~", var), design = design)
  
  # Using type III p-values to get a value for the overall value (rather than 
  # wald tests)
  
  car::Anova(model, type = "III") %>% slice_tail() 
  
}