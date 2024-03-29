#' Counting variables by BZD
#'
#' @param var 
#' @param byvar 
#' @param design 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
svy_countby <- function(var, 
                        byvar = bzd,
                        design = design.infl,
                        data){
  library(tidyverse)
  library(survey)
  
  form <- reformulate(as.character(paste0(var, "+", byvar)))
  
  if (is.character(var)) {
    var <- rlang::sym(var)
    var <- rlang::enquo(var)
  }
  if (is.character(byvar)) {
    byvar <- rlang::sym(byvar)
    byvar <- rlang::enquo(byvar)
  }
  
  # var_sym <- rlang::sym(var)
  # byvar_sym <- rlang::sym(byvar)


  
  # form <- reformulate(as.character(paste0(substitute(var), "+", substitute(byvar))))
  # 
  # var = enquo(var)
  # byvar = enquo(byvar)
  
  counts <- data %>%
    dplyr::group_by(!!var, !!byvar) %>% 
    count() %>%
    as.data.frame()
  
  percent <- svytable(formula = form, design = design) %>% 
    as.data.frame() %>% 
    dplyr::group_by(!!byvar) %>% 
    dplyr::mutate(
      n = sum(Freq),
      prop = Freq/n,
      percent = paste0(round(prop*100, 2), "%")
    ) %>% 
    dplyr::select(
      -prop, -Freq, -n
    )
  
  df <- counts %>%
    dplyr::left_join(
      percent
    ) %>% 
    dplyr::mutate(
      output = paste0(n, " (", percent, ")")
    ) %>% 
    dplyr::select(
      -n, -percent
    )
  
  return(df)
  
}
