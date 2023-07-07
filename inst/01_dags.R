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

#... Variables ----

# Might have to only say anxiety and BZD is an association relationship because
# anxiety can cause BZDs, therefore not making the DAG acyclic

dag = ggdag::dagify(
  anxiety ~ bzd + age + ms + inc, 
  bzd ~ age + sex + ms + ss + edu + inc, 
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + inc, 
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

#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

#... Open Paths ----

dag |> 
  ggdag_paths(from = "bzd", to = "anxiety")

#... D-seperated ----

# dag |> 
#   ggdag::ggdag_dseparated(controlling_for = "age")

# Depression  ----

#... Variables ----

dag = ggdag::dagify(
  depression ~ bzd + ms + inc, 
  bzd ~ age + sex + ms + ss + edu + inc, 
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
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
    inc = "Total Household\n Income",
    stress = "Stress"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Diabetes Mellitus  ----

#... Variables ----

ag = ggdag::dagify(
  diabetes ~ bzd + age + weight_gain,
  
  
  
  
  overweight ~ nutrition + inc , 
  
  
  
  
  
  
  diabetes ~ bzd + ms + inc, 
  bzd ~ age + sex + ms + ss + edu + inc, 
  # nothing causes age or sex 
  ms ~ age + stress, 
  ss ~ stress + inc, 
  edu ~ age,
  inc ~ edu + age + sex + ms,
  stress ~ age + ms + edu + inc,
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
    stress = "Stress",
    weight_gain = "Weight Gain", 
    diet = "Diet"
    # insomnia = "Insomnia", (assuming anxiety causes insomnia)
    # prescriber = "Type of Prescriber\n (GP vs Psychiatrist)"
  )
) 


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# High Blood Pressure  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Myocardial Infarction  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Stroke  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Cancer  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Heart Disease  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# COPD  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Dementia  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Infections  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")

# Pneumonia  ----

#... Variables ----


#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set(text = FALSE, 
                       use_labels = "label")
