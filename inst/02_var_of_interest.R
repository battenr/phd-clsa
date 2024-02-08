# Selecting Covariates and Outcomes (aka Selecting Variables of Interest)

library(tidyverse)
library(janitor)

# Selecting Covariates ----

source("inst/00_loading_data.R")

# Covariates associated with BZD use: 
# age, sex, marital status, smoking status, education, income, region of canada

dat |> count(sdc_urban_rural_com)

dat_var <- dat %>% 
  dplyr::select(
    entity_id, # participant id
    sex_ask_com, # gender
    age_nmbr_com, # age number
    age_grp_com, # age group
    # dosage
    # duration of prescription
    wghts_prov_com, # province
    wghts_analytic_com,
    wghts_inflation_com,
    geostrata_com,
    
    wea_mrtl_current_mcq, # marital status (current marital status)
    icq_smoke_com, # smoking status (what is your current smoking status?)
    ed_high_com,  # highest level of education completed
    ed_high_otsp_com, # highest education - other, please specify
    inc_tot_com,# income (total household income)
    sdc_urban_rural_com # urban/rural status
  ) %>% 
  dplyr::mutate(
    entity_id = as.character(entity_id) 
  ) 

dat_var = dat_var %>% 
  dplyr::filter(
    is.na(wghts_inflation_com) != TRUE # removing one participant due to NA value for WGHTS
  ) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    province_name = case_when(
      wghts_prov_com == 1 ~ "Alberta",
      wghts_prov_com == 2 ~ "British Columbia",
      wghts_prov_com == 3 ~ "Manitoba",
      wghts_prov_com == 4 ~ "New Brunswick",
      wghts_prov_com == 5 ~ "Newfoundland and Labrador",
      wghts_prov_com == 6 ~ "Nova Scotia",
      wghts_prov_com == 7 ~ "Ontario",
      wghts_prov_com == 8 ~ "Prince Edward Island",
      wghts_prov_com == 9 ~ "Quebec",
      wghts_prov_com == 10 ~ "Saskatchewan"
    )
  )


# Formatting Data ----

dat_output = dat_var %>% 
  rename_at(.vars = vars(ends_with("_com")),
            .funs = funs(sub("[_]com$", "", .))) %>%  # removing "_com" from all variable
  # Renaming variables
  dplyr::mutate(
    age_group = case_when(
      age_grp == 1 ~ "45-54",
      age_grp == 2 ~ "55-64",
      age_grp == 3 ~ "65-74",
      age_grp == 4 ~ "75+"
    ),
    marital_status = case_when(
      wea_mrtl_current_mcq == 1 ~ "Single, never married or never lived with a partner",
      wea_mrtl_current_mcq == 2 ~ "Married / Living with a partner in a common-law relationship",
      wea_mrtl_current_mcq == 3 ~ "Widowed",
      wea_mrtl_current_mcq == 4 ~ "Divorced",
      wea_mrtl_current_mcq == 5 ~ "Separated",
      wea_mrtl_current_mcq == 8 ~ "Don't know / No Answer",
      wea_mrtl_current_mcq == 9 ~ "Refused",
      wea_mrtl_current_mcq == -8 ~ "Missing"
    ),
    smoking_status = case_when(
      icq_smoke == 1 ~ "Yes (I currently smoke)",
      icq_smoke == 2 ~ "No (I don't smoke and I never have)",
      icq_smoke == 3 ~ "Former (I don't smoke now but I have in the past)",
      icq_smoke == -8 ~ "Missing - Technical"
    ),
    edu_high = case_when(
      ed_high == "01" ~ "No post-secondary degree, certificate or diploma",
      ed_high == "02" ~ "Trade certificate or diploma from a vocational school or apprenticeship training",
      ed_high == "03" ~ "Non-university certificate or diploma from a community college, CEGEP, school of nursing, etc.",
      ed_high == "04" ~ "University certificate below bachelor's level",
      ed_high == "05" ~ "Bachelor's degree",
      ed_high == "06" ~ "University degree or certificate above bachelor's degree",
      ed_high == "97" ~ "Other (please specify)",
      ed_high == "98" ~ "[DO NOT READ] Don't know / No answer",
      ed_high == "99" ~ "[DO NOT READ] Refused"
    ),
    total_income = case_when(
      inc_tot == 1 ~ "Less than $20,000", 
      inc_tot == 2 ~ "$20,000 or more but less than $50,000", 
      inc_tot == 3 ~ "$50,000 or more but less than $100,000",
      inc_tot == 4 ~ "$100,000 or more but less than $150,000",
      inc_tot == 5 ~ "$150,000 or more",
      inc_tot == 8 ~ "[DO NOT READ] Don't know / No answer",
      inc_tot == 9 ~ "[DO NOT READ] Refused"
    ),
    senior = dplyr::case_when(
      age_nmbr >= 65 ~ "yes",
      age_nmbr < 65 ~ "no"
    ),
    urban_rural_classification = dplyr::case_when(
      sdc_urban_rural == 0 ~ "Rural", 
      sdc_urban_rural == 1 ~ "Urban core",
      sdc_urban_rural == 2 ~ "Urban fringe",
      sdc_urban_rural == 4 ~ "Urban population centre outside CMA and CA",
      sdc_urban_rural == 6 ~ "Secondary core",
      sdc_urban_rural == 9 ~ "Postal code link to dissemination area"
    )
  ) %>% 
  dplyr::select(
    -c(age_grp, icq_smoke, wea_mrtl_current_mcq, inc_tot, ed_high)
  )

# Adding names to variables ----

df_output <- dat_output %>% 
  select(
    entity_id, 
    sex_ask,
    age_nmbr, 
    wghts_prov, 
    wghts_analytic, 
    wghts_inflation, 
    geostrata, 
    province_name, 
    age_group, 
    marital_status, 
    smoking_status, 
    edu_high, 
    total_income,
    urban_rural_classification
  )

vars <- df_output %>% colnames()

var_names <- c(
  "Unique subject identifer", 
  "Sex asked", 
  "Age as a number", 
  "Provincal weight",
  "Analytic weight",
  "Inflation weight",
  "Geostrata",
  "Province",
  "Age Group", 
  "Current marital status",
  "Smoking status", 
  "Education - highest degree",
  "Total household income",
  "Urban/Rural Classification"
)

dat_output %>% colnames()

for (x in 1:length(vars)){
  var <- vars[x]
  var_label <- var_names[x]
  
  attr(df_output[[var]], "label") <- var_label # adding attributes to data
  
}

df_covars <- df_output

# Saving as csv for loading into RMarkdown -----

save(df_covars, file = "data/df_covars.RData")


