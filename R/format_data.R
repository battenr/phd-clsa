

format_data <- function(dat = df) {
  dat %>% 
  mutate(centered_age = age - 58) %>% # subtracting mean from age 
  
  # Converting the values of "yes" and "no" to 1 and 0 so 
  # they can be used in the logistic regression
  
  mutate(
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
  ) %>%
  
  dplyr::select(
    bzd,
    age, 
    centered_age,
    sex,
    province,
    region,
    marital_status,
    smoke,
    education,
    income,
    #income,
    urban_rural,
    
    # Depression
    
    depression,
    
    # 
    
    entity_id,
    
    wghts_analytic, 
    geostrata
    
  ) %>%
  
  # Dropping NA 
  
  drop_na() %>% 
  
  # Grouping to create covariate patterns 
  
  group_by(bzd, 
           age, 
           sex, 
           region, 
           marital_status, 
           smoke, 
           income) %>% 
  # Adding group ID 
  mutate(
    mj = cur_group_id()
  ) %>% 
  
  ungroup() #%>% 
}
