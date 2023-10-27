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
      ccc_anxi_com == 1 ~ "yes", 
      ccc_anxi_com != 1 ~ "no"
    ),
    
   depression = dplyr::case_when(
     dpr_clindep_com == 1 ~ "yes", 
     dpr_clindep_com != 1 ~ "no"
    ),
    
    diabetes_mellitus = dplyr::case_when(
      dia_type_com == 2 ~ "yes", 
      dia_type_com != 2 ~ "no"
    ),
   
   hbp = dplyr::case_when(
     ccc_hbp_com == 1 ~ "yes", 
     ccc_hbp_com != 1 ~ "no"
   ),
   
   mi = dplyr::case_when(
     ccc_ami_com == 1 ~ "yes",
     ccc_ami_com != 1 ~ "no"
   ),
   
   # ccc_cva_com
   
   stroke = dplyr::case_when(
     ccc_cva_com == 1 ~ "yes",
     ccc_cva_com != 1 ~ "no"
   ),
   
   cancer = dplyr::case_when(
     ccc_canc_com == 1 ~ "yes",
     ccc_canc_com != 1 ~ "no"
   ),
   
   heart_disease = dplyr::case_when(
     ccc_heart_com == 1 ~ "yes",
     ccc_heart_com != 1 ~ "no"
   ),
   
   copd = dplyr::case_when(
     ccc_copd_com == 1 ~ "yes",
     ccc_copd_com != 1 ~ "no"
   ),
   
   dementia = dplyr::case_when(
     ccc_alzh_com == 1 ~ "yes",
     ccc_alzh_com != 1 ~ "no"
   ),
   
  pneumonia = dplyr::case_when(
    ccc_drpneu_com == 1 ~ "yes",
    ccc_drpneu_com != 1 ~ "no"
  )
    
   
    
  ) %>% 
  dplyr::select(
    entity_id,
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
