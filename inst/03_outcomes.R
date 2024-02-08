# Outcomes of Interest

# Anxiety: ccc_anxi

# Depression: dpr_clindep_com (Clinical Depression)

# Diabetes Mellitus:

# Do you have diabetes: yes/no
# Type: dia_type_com 

# High blood pressure

# CCC_hbp_com

# Myocardial Infarction

# ccc_ami_com

# Stroke

# ccc_cva_com

# Cancer

# CCC_CANC_COM

# Heart Disease

# CCC_HEART_COM

# COPD

# CCC_COPD_COM

# Dementia 

# ccc_alzh_com # would assume it's none 

# Infections

# Requries more thought. There are eyes ears and others

# Pneumonia 

# In the past year have you seen a doctor for pneumonia (needs more thinking)

# CCC_DRPNEU_COM

dat$ccc_mi

source("inst/00_loading_data.R")

df_outcomes <- dat %>% 
  dplyr::select(
    entity_id, 
    ccc_anxi_com, 
    dpr_clindep_com,
    dia_type_com, # There are some that are NA. So those that say "yes" to diabetes
    # but type is missing I'm not including as Type 2 diabetes
    
    # Infections: TBD
    
    ccc_hbp_com,
    ccc_ami_com,
    ccc_cva_com,
    ccc_canc_com,
    ccc_heart_com,
    ccc_copd_com,
    ccc_alzh_com,
    ccc_drpneu_com
    
    # icq_eyeinf_com,
    # icq_earinf_com,
    
  ) %>% 
  
  dplyr::mutate(
    anxiety = dplyr::case_when(
      ccc_anxi_com == 1 ~ "Yes", 
      ccc_anxi_com == 2 ~ "No",
      ccc_anxi_com == 8 ~ "Don't know or No answer",
      ccc_anxi_com == 9 ~ "Refused"
    ),
    
   depression = dplyr::case_when(
     dpr_clindep_com == 1 ~ "Yes", 
     dpr_clindep_com == 2 ~ "No",
     dpr_clindep_com == 8 ~ "Don't know or No answer",
     dpr_clindep_com == 9 ~ "Refused"
    ),
    
    diabetes_type = dplyr::case_when(
      dia_type_com == 1 ~ "Type I", 
      dia_type_com == 2 ~ "Type II",
      dia_type_com == 3 ~ "Neither",
      dia_type_com == 8 ~ "Don't know or No answer",
      dia_type_com == 9 ~ "Refused"
    ),
   
   hbp = dplyr::case_when(
     ccc_hbp_com == 1 ~ "Yes", 
     ccc_hbp_com == 2 ~ "No",
     ccc_hbp_com == 8 ~ "Don't know or No answer",
     ccc_hbp_com == 9 ~ "Refused"
   ),
   
   mi = dplyr::case_when(
     ccc_ami_com == 1 ~ "Yes", 
     ccc_ami_com == 2 ~ "No",
     ccc_ami_com == 8 ~ "Don't know or No answer",
     ccc_ami_com == 9 ~ "Refused"
   ),
   
   # ccc_cva_com
   
   stroke = dplyr::case_when(
     ccc_cva_com == 1 ~ "Yes", 
     ccc_cva_com == 2 ~ "No",
     ccc_cva_com == 8 ~ "Don't know or No answer",
     ccc_cva_com == 9 ~ "Refused"
   ),
   
   cancer = dplyr::case_when(
     ccc_canc_com == 1 ~ "Yes", 
     ccc_canc_com == 2 ~ "No",
     ccc_canc_com == 8 ~ "Don't know or No answer",
     ccc_canc_com == 9 ~ "Refused"
   ),
   
   heart_disease = dplyr::case_when(
     ccc_heart_com == 1 ~ "Yes", 
     ccc_heart_com == 2 ~ "No",
     ccc_heart_com == 8 ~ "Don't know or No answer",
     ccc_heart_com == 9 ~ "Refused"
   ),
   
   copd = dplyr::case_when(
     ccc_copd_com == 1 ~ "Yes", 
     ccc_copd_com == 2 ~ "No",
     ccc_copd_com == 8 ~ "Don't know or No answer",
     ccc_copd_com == 9 ~ "Refused"
   ),
   
   dementia = dplyr::case_when(
     ccc_alzh_com == 1 ~ "Yes", 
     ccc_alzh_com == 2 ~ "No",
     ccc_alzh_com == 8 ~ "Don't know or No answer",
     ccc_alzh_com == 9 ~ "Refused"
   ),
   
  pneumonia = dplyr::case_when(
    ccc_drpneu_com == 1 ~ "Yes", 
    ccc_drpneu_com == 2 ~ "No",
    ccc_drpneu_com == 8 ~ "Don't know or No answer",
    ccc_drpneu_com == 9 ~ "Refused"
  )
    
   
    
  ) %>% 
  dplyr::select(
    entity_id,
    anxiety, 
    depression,
    diabetes_type,
    hbp, 
    mi, 
    stroke, 
    cancer, 
    heart_disease, 
    copd, 
    dementia, 
    # infections, # infections TBD
    pneumonia
  )

vars <- df_outcomes %>% colnames()

var_names <- c(
  "Unique subject identifer", 
  "Anxiety",
  "Depression",
  "Diabetes Mellitus",
  "High blood pressure",
  "Myocardial Infarction",
  "Stroke",
  "Cancer",
  "Heart Disease",
  "COPD",
  "Dementia", 
  #"Infections", # Infections TBD
  "Pneumonia"
)

for (x in 1:length(vars)){
  var <- vars[x]
  var_label <- var_names[x]
  
  attr(df_outcomes[[var]], "label") <- var_label # adding attributes to data
  
}

save(df_outcomes, file = "data/df_outcomes.Rdata")
