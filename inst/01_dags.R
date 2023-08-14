# Title: DAGs

# Description: Directed acyclic graphs for outcomes (DAGs)

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)

theme_set(theme_dag())

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAGs ----

# Adverse effects: 

# Variables of interest: 

# Goal is to use DAGs to inform what our adjustments should look like. Using causal diagrams will allow to adjust for 
# potential bias while also avoiding additional biases such as adjusting for a collider and causing
# an open path

# Covariates associated with BZD use: 
# age, sex, marital status, smoking status, education, income, region of canada

# Anxiety  ----

#... Variables 

# Might have to only say anxiety and BZD is an association relationship because
# anxiety can cause BZDs, therefore not making the DAG acyclic

dag = ggdag::dagify(
  anxiety ~ bzd + age + ms + inc, 
  bzd ~ age + sex + ms + ss + edu + inc, 
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
  exposure = "bzd",
  outcome = "anxiety",
  labels = c(
    anxiety = "Anxiety",
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    stress = "Stress"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

#... Open Paths 

dag |> 
  ggdag_paths(from = "bzd", to = "anxiety")

#... D-seperated 

# dag |> 
#   ggdag::ggdag_dseparated(controlling_for = "age")

# Depression  ----

#... Variables 

# Need to decide if we will include stress. Does stress cause BZD use? 
# Debatable, good idea to ask committee. Stress -> anxiety -> BZD? or 
# Stress -> BZD?

dag = ggdag::dagify(
  depression ~ bzd + ms + inc + age, # + stress, 
  bzd ~ age + sex + ms + ss + edu + inc, # + stress, 
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  #stress ~ age + ms + edu + inc,
  exposure = "bzd",
  outcome = "depression",
  labels = c(
    depression = "Depression",
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income"#,
    #stress = "Stress"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 


#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Diabetes Mellitus  ----

#... Variables 

dag = ggdag::dagify(
  diabetes ~ bzd + age + sex + ss + weight_gain,
  bzd ~ age + sex + ms + ss + edu + inc,
  # nothing causes age or sex 
  ms ~ age, 
  ss ~ edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  #depression ~ age + ms + inc, 
  weight_gain ~ age + sex + ms + edu + inc + ms,
  exposure = "bzd",
  outcome = "diabetes",
  labels = c(
    diabetes = "Diabetes\nMellitus",
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    weight_gain = "Weight Gain"
  )
) 


#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# High Blood Pressure  ----

# link: https://www.nhs.uk/conditions/high-blood-pressure-hypertension/causes/

# Can stress even be included? It's not well defined 

#... Variables 

# Note: don't need to include stress or anxiety b/c they aren't common causes

dag = ggdag::dagify(
  hbp ~ anxiety + age + sex + ss, # + stress, #+ overweight,
  bzd ~ age + sex + ms + ss + edu + inc + anxiety, # + anxiety,
  # nothing causes age or sex 
  ms ~ age, #+ stress, 
  ss ~ edu + inc, # + stress, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  #stress ~ age + ms + edu + inc,
  #overweight ~ age + sex + ms + edu + inc,
  #anxiety ~ stress + ms + inc,
  anxiety ~ age + ms + inc,
  exposure = "bzd",
  outcome = "hbp",
  labels = c(
    hbp = "High Blood\n Pressure", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income"#,
    #stress = "Stress",
    #overweight = "Overweight",
    #anxiety = "Anxiety"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Myocardial Infarction  ----

#... Variables 

dag = ggdag::dagify(
  mi ~ bzd + age + sex + ss + hbp + anxiety, #stress + overweight + hbp,
  bzd ~ age + sex + ms + ss + edu + inc + anxiety, # + stress,
  # nothing causes age or sex 
  ms ~ age, # + stress, 
  ss ~ edu + inc, #stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  #stress ~ age + ms + edu + inc,
  #overweight ~ age + sex + ms + edu + inc,
  hbp ~ anxiety + age + sex + ss,
  anxiety ~ age + ms + inc,
  exposure = "bzd",
  outcome = "mi",
  labels = c(
    mi = "Myocardial\nInfarction", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    anxiety = "Anxiety",
    #stress = "Stress",
    #overweight = "Overweight",
    hbp = "High Blood Pressure"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Stroke  ----

#... Variables 

dag = ggdag::dagify(
  stroke ~ age + sex + ss + hbp,
  bzd ~ age + sex + ms + ss + edu + inc, # + stress,
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  # stress ~ age + ms + edu + inc,
  hbp ~ anxiety + age + sex + ss,
  anxiety ~ bzd + age + ms + inc,
  exposure = "bzd",
  outcome = "stroke",
  labels = c(
    stroke = "Stroke", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    hbp = "High Blood\nPressure",
    anxiety = "Anxiety"
    # cvd = "Cardiovascular\nDisease"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 



#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Cancer  ----

#... Variables 

# Can only do associational odds ratios. Too complicated 

# Heart Disease  ----

#... Variables 

dag = ggdag::dagify(
  cvd ~ age + sex + ss + stress + overweight + hbp,
  bzd ~ age + sex + ms + ss + edu + inc + stress,
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
  overweight ~ age + sex + ms + edu + inc,
  hbp ~ overweight + stress + ss + sex
  exposure = "bzd",
  outcome = "cvd",
  labels = c(
    cvd = "Heart\nDisease", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    stress = "Stress",
    overweight = "Overweight",
    hbp = "High Blood Pressure"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 


#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# COPD  ----

#... Variables 

dag = ggdag::dagify(
  
  
  
  
  copd ~ age + sex + ss + overweight + rural,
  bzd ~ age + sex + ms + ss + edu + inc + rural,
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
  overweight ~ age + sex + ms + edu + inc,
  rural ~ age + sex + inc + ms, 
  exposure = "bzd",
  outcome = "copd",
  labels = c(
    copd = "COPD", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    stress = "Stress",
    overweight = "Overweight",
    rural = "Living in \nRural Area" # this may be tricky. May not actually be rural that causes
    # COPD but rather undiagnosed or something
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Dementia  ----

#... Variables 

dag = ggdag::dagify(
  dementia ~ age + ss + hbp + diabetes + overweight + stress,
  bzd ~ age + sex + ms + ss + edu + inc + stress,
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
  overweight ~ age + sex + ms + edu + inc,
  diabetes ~ overweight + age + sex + inc,
  hbp ~ overweight + stress + ss + sex,
  exposure = "bzd",
  outcome = "dementia",
  labels = c(
    dementia = "Dementia", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    stress = "Stress",
    overweight = "Overweight",
    hbp = "High Blood Pressure"
    # this may be tricky. May not actually be rural that causes
    # COPD but rather undiagnosed or something
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Infections  ----

#... Variables 



#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Pneumonia  ----

#... Variables 

dag = ggdag::dagify(
  pneumonia ~ age + ss + overweight, # need to lookup more risk factors
  bzd ~ age + sex + ms + ss + edu + inc + stress,
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + edu + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
  overweight ~ age + sex + ms + edu + inc,
  # diabetes ~ overweight + age + sex + inc,
  # hbp ~ overweight + stress + ss + sex,
  exposure = "bzd",
  outcome = "pneumonia",
  labels = c(
    pneumonia = "Pneumonia", 
    bzd = "Benzodiazepine\nUse", 
    age = "Age",
    sex = "Sex",
    ms = "Marital\nStatus",
    ss = "Smoking\nStatus",
    edu = "Education",
    inc = "Total Household\n Income",
    stress = "Stress",
    overweight = "Overweight",
    hbp = "High Blood Pressure"
    # this may be tricky. May not actually be rural that causes
    # COPD but rather undiagnosed or something
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 

#... Draw DAG 

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set 

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")
