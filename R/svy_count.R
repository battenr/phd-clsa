svy_count <- function(var, 
                      design = design.infl,
                      data){
  library(tidyverse)
  library(survey)
  
  # Removed as.name(substitute(var)) because it may not work well with lapply
  # var = as.name(substitute(var))
  
  counts <- data %>%
    count(!!sym(var)) %>%
    as.data.frame()
  
  form <- reformulate(var)  # Directly using var here
  
  percent <- svytable(formula = form, design = design) %>% 
    as.data.frame() %>% 
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
