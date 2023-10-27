# Flagging Benzodiazepines

# ATC/DIN Codes ----

# only using those with the status "Marketed"

bzd_codes <- tribble(
  ~din, ~drug_name, ~ atc, ~strength, # note: removed leading 0's from DIN
  # diazepam
  "2238162", "diazepam", "N05BA01", "5 mg/mL",
  "362158", "diazepam", "N05BA01", "5 mg",
  "405329", "diazepam", "N05BA01", "2 mg",
  "405337", "diazepam", "N05BA01", "10 mg",
  "399728", "diazepam", "N05BA01", "10 mg / 2 mL",
  "2385392", "diazepam", "N05BA01", "5 mg/mL",
  "13285", "diazepam", "N05BA01", "5 mg",
  # chlordiazepoxide
  "522724", "chlordiazepoxide", "N05BA02", "5 mg",
  "522988", "chlordiazepoxide", "N05BA02", "10 mg",
  "522996", "chlordiazepoxide", "N05BA02", "25 mg",
  # medazepam, none available for ATC N05BA03
  # oxazepam
  "402680", "oxazepam", "N05BA04", "10 mg",
  "402745", "oxazepam", "N05BA04", "15 mg",
  "402737", "oxazepam", "N05BA04", "30 mg",
  "497754", "oxazepam", "N05BA04", "10 mg", 
  "497762", "oxazepam", "N05BA04", "15 mg",
  "497770", "oxazepam", "N05BA04", "30 mg",
  "568392", "oxazepam", "N05BA04", "10 mg",
  "568406", "oxazepam", "N05BA04", "15 mg",
  "568414", "oxazepam", "N05BA04", "30 mg",
  # potassium clorazepate
  "860689", "potassium clorazepate", "N05BA05", "3.75 mg", 
  "860697", "potassium clorazepate", "N05BA05", "15 mg",
  "860700", "potassium clorazepate", "N05BA05", "7.5 mg",
  # lorazepam"
  "655740", "lorazepam", "N05BA06", "0.5 mg",
  "655759", "lorazepam", "N05BA06", "1 mg",
  "655767", "lorazepam", "N05BA06", "2 mg",
  "2041413", "lorazepam", "N05BA06", "0.5 mg",
  "2041421", "lorazepam", "N05BA06", "1 mg",
  "2041448", "lorazepam", "N05BA06", "2 mg",
  "2041456", "lorazepam", "N05BA06", "0.5 mg",
  "2041464", "lorazepam", "N05BA06", "1 mg",
  "2041472", "lorazepam", "N05BA06", "2 mg",
  "2351080", "lorazepam", "N05BA06", "1 mg",
  "2351099", "lorazepam", "N05BA06", "2 mg",
  "2243278", "lorazepam", "N05BA06", "4 mg / 1 mL",
  "2388669", "lorazepam", "N05BA06", "4 mg / 1 mL",
  "2438704", "lorazepam", "N05BA06", "2 mg / 1 mL",
  "2410745", "lorazepam", "N05BA06", "0.5 mg",
  "2410753", "lorazepam", "N05BA06", "1 mg",
  "2410761", "lorazepam", "N05BA06", "2 mg",
  "728187", "lorazepam", "N05BA06", "0.5 mg",
  "728195", "lorazepam", "N05BA06", "1 mg",
  "728209", "lorazepam", "N05BA06", "2 mg",
  "644643", "lorazepam", "N05BA06", "0.5 mg",
  "655651", "lorazepam", "N05BA06", "1 mg",
  "655678", "lorazepam", "N05BA06", "2 mg",
  "637742", "lorazepam", "N05BA06", "1 mg",
  "637750", "lorazepam", "N05BA06", "2 mg",
  "711101", "lorazepam", "N05BA06", "0.5 mg",
  # adinazolam, none availabe for ATC: N05BA07
  # bromazepam
  "2177161", "bromazepam", "N05BA08", "3 mg",
  "2177188", "bromazepam", "N05BA08", "6 mg",
  "2230584", "bromazepam", "N05BA08", "3 mg",
  "2230585", "bromazepam", "N05BA08", "6 mg",
  #clobazam
  "2244638", "clobazam", "N05BA09", "10 mg",
  "2238334", "clobazam", "N05BA09", "10 mg",
  # ketazolam, none available for ATC: N05BA10
  # prazepam, none available for ATC: N05BA11
  # alprazolam
  "2248706", "alprazolam", "N05BA12", "1 mg",
  "2349191", "alprazolam", "N05BA12", "0.25 mg",
  "2349205", "alprazolam", "N05BA12", "0.50 mg",
  "1908189", "alprazolam", "N05BA12", "0.25 mg",
  "1908170", "alprazolam", "N05BA12", "0.50 mg",
  "865397", "alprazolam", "N05BA12", "0.25 mg",
  "865400", "alprazolam", "N05BA12", "0.50 mg",
  "2243611", "alprazolam", "N05BA12", "1 mg",
  "2243612", "alprazolam", "N05BA12", "2 mg",
  "1913484", "alprazolam", "N05BA12", "0.25 mg",
  "1913492", "alprazolam", "N05BA12", "0.50 mg",
  "548359", "alprazolam", "N05BA12", "0.25 mg",
  "548367", "alprazolam", "N05BA12", "0.50 mg",
  "723770", "alprazolam", "N05BA12", "0.50 mg",
  "813958", "alprazolam", "N05BA12", "2 mg",
  # halazepam, none available for ATC: N05BA13
  # pinazepam, none available for ATC: N05BA14
  # camazepam, none available for ATC: N05BA15
  # nordazepam, none available for ATC: N05BA16
  # fludiazepam, none available for ATC: N05BA17
  # ethyl loflazepate, none available for ATC:	N05BA18
  # etizolam, none available for ATC:	N05BA19
  # clotiazepam, none available for ATC:	N05BA21
  # cloxazolam, none available for ATC:	N05BA22
  # tofisopam, none available for ATC:	N05BA23
  # bentazepam, none available for ATC:	N05BA24
  # lorazepam, combinations	N05BA56
  # clonazepam
  "2177889", "clonazepam", "N03AE01", "0.50 mg",
  "2177897", "clonazepam", "N03AE01", "2 mg",
  "2048701", "clonazepam", "N03AE01", "0.50 mg",
  "2048728", "clonazepam", "N03AE01", "1 mg",
  "2048736", "clonazepam", "N03AE01", "2 mg",
  "2179660", "clonazepam", "N03AE01", "0.25 mg",
  "2207818", "clonazepam", "N03AE01", "0.50 mg",
  "2311593", "clonazepam", "N03AE01", "0.50 mg",
  "2311607", "clonazepam", "N03AE01", "1 mg",
  "2311615", "clonazepam", "N03AE01", "2 mg",
  "2242077", "clonazepam", "N03AE01", "0.5 mg",
  "2242078", "clonazepam", "N03AE01", "2 mg",
  "382825", "clonazepam", "N03AE01", "0.5 mg",
  "382841", "clonazepam", "N03AE01", "2 mg",
  "2239024", "clonazepam", "N03AE01", "0.5 mg",
  "2239025", "clonazepam", "N03AE01", "2 mg",
  # flurazepam
  "521698", "flurazepam", "N05CD01", "15 mg",
  "521701", "flurazepam", "N05CD01", "30 mg",
  # nitrazepam
  "511528", "nitrazepam", "N05CD02", "5 mg", 
  "511536", "nitrazepam", "N05CD02", "10 mg",
  # flunitrazepam, none available for ATC:	N05CD03
  # estazolam, none available for ATC: N05CD04
  "808571", "triazolam", "N05CD05", "0.25 mg",
  # lormetazepam, none available for ATC: N05CD06
  "604453", "temazepam", "N05CD07", "15 mg",
  "604461", "temazepam", "N05CD07", "30 mg",
  # midazolam 
  "2240285", "midazolam", "N05CD08", "1 mg / mL",
  "2240286", "midazolam", "N05CD08", "5 mg / mL",
  "2242904", "midazolam", "N05CD08", "1 mg / mL",
  "2242905", "midazolam", "N05CD08", "5 mg / mL",
  "2423758", "midazolam", "N05CD08", "1 mg / mL",
  "2423766", "midazolam", "N05CD08", "5 mg / mL",
  "2382342", "midazolam", "N05CD08", "1 mg / mL",
  "2382350", "midazolam", "N05CD08", "5 mg / mL",
  "2382377", "midazolam", "N05CD08", "1 mg / mL",
  "2382385", "midazolam", "N05CD08", "5 mg / mL",
  # brotizolam, none available for ATC:	N05CD09
  # quazepam, none available for ATC:	N05CD10
  # loprazolam, none available for ATC: N05CD11
  # doxefazepam, none available for ATC: N05CD12
  # cinolazepam, none available for ATC: N05CD13
)

# Cleaning Medication Data ----

#... Selecting only Drug Identification Numbers variables ----

dat_din <- dat %>% 
  dplyr::select(
    entity_id,
    wghts_analytic_com,
    wghts_inflation_com,
    wghts_prov_com,
    starts_with("medi_id_din")
  )

dat_din %>% colnames()

#... ATC Codes ----

dat_atc <- dat %>% 
  dplyr::select(
    entity_id,
    wghts_analytic_com,
    wghts_inflation_com,
    wghts_prov_com,
    starts_with("medi_id_atc")
  )

# Filtering Medications ----

# The process for doing this is to first check if a value is in the list of 
# drug identification numbers (DINs) in the above bzd_codes data frame. If it is 
# then it receives a value of 1, if not then it receives a value of 0. 

# Then one by one, across each row, the columns are summed. If the value is 
# greater than 0, they are considered a BZD user. If not, then they are considered a non-BZD user

# This process is repeated for ATC codes. 

# Then a patient needs to be a "non-user" in both dataframe to be a "non-user" overall

#... By DIN ----

dat_bzd_din <- dat_din %>% 
  dplyr::mutate(
    across(
      .cols = starts_with("medi_id_din_sp2_"),
      ~as.character(.x)
    )) %>% 
  dplyr::mutate( # simpler way: using across instead of all of these 
    dplyr::across(
      .cols = starts_with("medi_id_din_sp2_"),
      ~ ifelse(.x %in% bzd_codes$din, 1, 0), # making 1, 0 as logical for later
      .names = "bzd_{col}"
    )
  ) %>% 
  rowwise() %>% 
  dplyr::mutate(
    bzd_sum = sum(across(
      .cols = starts_with("bzd") 
      
    ))
  )

dat_bzd_din %>% select(entity_id, bzd_sum) %>% view()

nrow(dat_bzd_din$bzd_sum > 1)/30097

dat_bzd_din %>% filter(bzd_sum >= 1) %>% nrow()

1035/30097

bzd_flag_din <- dat_bzd_din %>% 
  dplyr::mutate(
    bzd_flag = dplyr::case_when(
      bzd_sum >= 1 ~ "yes", 
      bzd_sum == 0 ~ "no"
    )
  ) %>% 
  select(entity_id, bzd_flag)

rm(dat_bzd_din, dat_din, bzd_din)

#... By ATC ----

dat_bzd_atc <- dat_atc %>% 
  dplyr::mutate(
    across(
      .cols = starts_with("medi_id_atc_"),
      ~as.character(.x)
    )) %>% 
  dplyr::mutate( # simpler way: using across instead of all of these 
    dplyr::across(
      .cols = starts_with("medi_id_atc_"),
      ~ ifelse(.x %in% bzd_codes$atc, 1, 0), # making 1, 0 as logical for later
      .names = "bzd_{col}"
    )
  ) %>% 
  rowwise() %>% 
  dplyr::mutate(
    bzd_sum = sum(across(
      .cols = starts_with("bzd") 
      
    ))
  )

dat_bzd_atc %>% filter(bzd_sum >= 1) %>% nrow()

1311/30097

1035/30097



bzd_flag_atc <- dat_bzd_atc %>% 
  dplyr::mutate(
    bzd_atc = dplyr::case_when(
      bzd_sum >= 1 ~ "yes", 
      bzd_sum == 0 ~ "no"
    )
  ) %>% 
  select(entity_id, bzd_atc)



#... Filtering dataset to only include BZDs -----

typeof(dat_med_bzd$medi_id_din_sp2_1_com)

dat_med_din %>% colnames()



#----- Benzodiazepines in CLSA ------------------------------

str(bzd_flag_atc)

bzd_flag_din_and_atc <- bzd_flag_din %>% 
  dplyr::left_join(
    bzd_flag_atc
  ) %>% 
  dplyr::mutate(
    bzd = case_when(
      bzd_flag == "yes" | bzd_atc == "yes" ~ "yes",
      bzd_flag == "no" & bzd_atc == "no" ~ "no"
    )
  )


bzd_flag <- bzd_flag_din_and_atc %>% 
  dplyr::select(
    entity_id, 
    bzd
  )

bzd_flag %>% count(bzd)

1311/30097


# Counting how many bzd users

bzd.users = dat_bzd %>% filter(bzd_any == "yes") %>% nrow()
total.n = dat_bzd %>% nrow()

round(bzd.users/total.n * 100, 2) # 3.43%, similar to what has been 
# reported by Esposito et al. (2009)

save(bzd_flag, file = "data/bzd_flag.Rdata")
