# Descriptive Stats ---

# First we need to load our data 

library(tidyverse)

# Data ----

load("data/bzd_flag.Rdata")
load("data/df_covars.RData")
load("data/df_outcomes.Rdata")

df = bzd_flag %>% 
  mutate(entity_id = as.character(entity_id)) %>% 
  dplyr::left_join(
    df_covars %>%  mutate(entity_id = as.character(entity_id))
  ) %>% 
  dplyr::left_join(
    df_outcomes %>% mutate(entity_id = as.character(entity_id))
  ) 

# Analytic Datasets ----

df |> colnames()

# ... Variable Names ----

lapply(df, unique)


#.... Dataset ----

df.analytic <- df |> 
  
  dplyr::rename(
    sex = sex_ask, 
    age = age_nmbr,
    province = province_name
  ) |> 
  
  mutate(
    # Covariates 
     marital_status = dplyr::case_when(
       
       # Rationale is that married/spouse is different than always single, and different than separated
       # (separated could be you using BZDs to cope with anxiety from that)
       
       marital_status == NA ~ NA_character_,
       marital_status == "Missing" ~ NA_character_,
       marital_status == "Married / Living with a partner in a common-law relationship" ~ "married",
       marital_status == "Single, never married or never lived with a partner" ~ "single",
       marital_status == "Widowed" ~ "separated",                                                     
       marital_status == "Divorced" ~ "separated",                                                    
       marital_status == "Separated" ~ "separated",                                                   
       marital_status == "Refused" ~ NA_character_,                                                     
       marital_status == "Don't know / No Answer" ~ NA_character_
     ),
     
     smoke = dplyr::case_when(
       smoking_status == "Former (I don't smoke now but I have in the past)" ~ "Former",
       smoking_status == "No (I don't smoke and I never have)" ~ "No",
       smoking_status == "Yes (I currently smoke)" ~ "Yes",                          
       smoking_status == "Missing - Technical" ~ NA_character_
     ),
     
     education = dplyr::case_when(
       
       # Question is "What is the highest degree, certificate or diploma you have obtained?"
       
       edu_high == "No post-secondary degree, certificate or diploma" ~ "no post-secondary",                                              
       edu_high == "Bachelor's degree" ~ "post-secondary",                                                                             
       edu_high == "Trade certificate or diploma from a vocational school or apprenticeship training" ~ "post-secondary",             
       edu_high == "University degree or certificate above bachelor's degree" ~ "post-secondary",                                     
       edu_high == "Non-university certificate or diploma from a community college, CEGEP, school of nursing, etc." ~ "post-secondary",
       edu_high == "Other (please specify)" ~ "post-secondary", # rationale is based on question                                                                       
       edu_high == NA ~ NA_character_,                                                                                            
       edu_high == "University certificate below bachelor's level" ~ "post-secondary",                                               
       edu_high == "[DO NOT READ] Don't know / No answer" ~ NA_character_
     ),
     
     household_income = dplyr::case_when(
       total_income == "$20,000 or more but less than $50,000" ~ "20k to <50k",
       total_income == "Less than $20,000" ~ "<20k",                     
       total_income == "$50,000 or more but less than $100,000" ~ "50k to <100k", 
       total_income == "$100,000 or more but less than $150,000" ~ "100k to <150k",
       total_income == "$150,000 or more" ~ "150k+",                       
       total_income == "[DO NOT READ] Don't know / No answer" ~ NA_character_,
       total_income == "[DO NOT READ] Refused" ~ NA_character_
     ),
     
     urban_rural = dplyr::case_when(
       urban_rural_classification == "Urban core" ~ "urban",                             
       urban_rural_classification == "Rural" ~ "rural",                           
       urban_rural_classification == "Urban population centre outside CMA and CA" ~ "urban",
       urban_rural_classification == "Secondary core" ~ "urban",                            
       urban_rural_classification == "Postal code link to dissemination area" ~ NA_character_,   
       urban_rural_classification == "Urban fringe" ~ "urban"
     ),
     
     # Outcomes 
     
     anxiety = dplyr::case_when(
       anxiety == "Yes" ~ "yes",
       anxiety == "No" ~ "no",
       anxiety == "Don't know or No Answer" ~ NA_character_,
       anxiety == "Refused" ~ NA_character_,
       is.na(anxiety) == TRUE ~ NA_character_ 
     ), 
     depression = dplyr::case_when(
       depression == "Yes" ~ "yes",
       depression == "No" ~ "no",
       depression == "Don't know or No Answer" ~ NA_character_,
       depression == "Refused" ~ NA_character_,
       is.na(depression) == TRUE ~ NA_character_ 
     ), 
     diabetes_mellitus = dplyr::case_when(
       diabetes_type == "Neither" ~ "no",
       diabetes_type == "Type I" ~ "no",
       diabetes_type == "Type II" ~ "yes",
       diabetes_type == "Don't know or No Answer" ~ NA_character_,
       is.na(diabetes_type) == TRUE ~ NA_character_ 
     ), 
     
     # Paused HERE
     
     hbp = dplyr::case_when(
       hbp == "Yes" ~ "yes",
       hbp == "No" ~ "no",
       hbp == "Don't know or No Answer" ~ NA_character_,
       hbp == "Refused" ~ NA_character_,
       is.na(hbp) == TRUE ~ NA_character_ 
     ), 
     mi = dplyr::case_when(
       mi == "Yes" ~ "yes",
       mi == "No" ~ "no",
       mi == "Don't know or No Answer" ~ NA_character_,
       mi == "Refused" ~ NA_character_,
       is.na(mi) == TRUE ~ NA_character_ 
     ),  
     stroke = dplyr::case_when(
       stroke == "Yes" ~ "yes",
       stroke == "No" ~ "no",
       stroke == "Don't know or No Answer" ~ NA_character_,
       stroke == "Refused" ~ NA_character_,
       is.na(stroke) == TRUE ~ NA_character_ 
     ),  
     cancer = dplyr::case_when(
       cancer == "Yes" ~ "yes",
       cancer == "No" ~ "no",
       cancer == "Don't know or No Answer" ~ NA_character_,
       cancer == "Refused" ~ NA_character_,
       is.na(cancer) == TRUE ~ NA_character_ 
     ),  
     heart_disease = dplyr::case_when(
       heart_disease == "Yes" ~ "yes",
       heart_disease == "No" ~ "no",
       heart_disease == "Don't know or No Answer" ~ NA_character_,
       heart_disease == "Refused" ~ NA_character_,
       is.na(heart_disease) == TRUE ~ NA_character_ 
     ),  
     copd = dplyr::case_when(
       copd == "Yes" ~ "yes",
       copd == "No" ~ "no",
       copd == "Don't know or No Answer" ~ NA_character_,
       copd == "Refused" ~ NA_character_,
       is.na(copd) == TRUE ~ NA_character_ 
     ),  
     dementia = dplyr::case_when(
       dementia == "Yes" ~ "yes",
       dementia == "No" ~ "no",
       dementia == "Don't know or No Answer" ~ NA_character_,
       dementia == "Refused" ~ NA_character_,
       is.na(dementia) == TRUE ~ NA_character_ 
     ),  
     # infections, # infections TBD
     pneumonia = dplyr::case_when(
       pneumonia == "Yes" ~ "yes",
       pneumonia == "No" ~ "no",
       pneumonia == "Don't know or No Answer" ~ NA_character_,
       pneumonia == "Refused" ~ NA_character_,
       is.na(pneumonia) == TRUE ~ NA_character_ 
     )
     
  ) |> 
  
  dplyr::select(
    entity_id, 
    
    bzd, 
    age,
    sex,
    province,
    marital_status,
    smoke,
    education, 
    household_income,
    urban_rural,
    starts_with("wghts"),
    geostrata,
    
    # Outcomes 
    
    anxiety, 
    depression,
    diabetes_mellitus,
    hbp, 
    mi, 
    stroke,
    cancer,
    heart_disease, 
    copd, 
    dementia, 
    pneumonia
    
    
    
  )

# Labelling Variables -----

vars <- df %>% colnames()

var_names <- c(
  "Unique subject identifer", 
  "Benzodiazepine (Yes/No)",
  "Age",
  "Sex (M/F)",
  "Province",
  "Marital Status",
  "Smoking Status",
  "Education (Post secondary)",
  "Total Household Income",
  "Urban/Rural Status",
  "Provincal Weights",
  "Analytic Weights",
  "Inflation Weights",
  "Geostrata",
  "Anxiety",
  "Depression",
  "Type II Diabetes",
  "High Blood Pressure",
  "Myocardial Infarction",
  "Stroke",
  "Cancer (Any type)",
  "Heart disease",
  "COPD",
  "Dementia",
  "Pneumonia"
)

for (x in 1:length(vars)){
  var <- vars[x]
  var_label <- var_names[x]
  
  attr(df[[var]], "label") <- var_label # adding attributes to data
  
}

# Output ----

df = df.analytic

save(df, file = "data/analytic_dataset.Rdata")
