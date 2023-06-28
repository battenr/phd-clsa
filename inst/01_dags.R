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

dag = ggdag::dagify(
  anxiety ~ bzd + ms + stress, 
  bzd ~ age + sex + ms + ss + edu + inc + rc, 
  age ~ sex + ms + ss + edu + inc + rc,
  # nothing affects sex in this DAG
  ms ~  age + stress + religion + inc,
  ss ~ stress + edu + sex + inc, 
  edu ~ age + sex, 
  inc ~ edu + sex + rc,
  stress ~ inc, 
  religion ~ edu,
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
    inc = "Income",
    rc = "Region of\nCanada",
    stress = "Stress",
    religion = "Religion"
  )
) 

#... Draw DAG ----

dag %>% 
  ggdag::ggdag(layout = "circle", 
               text = FALSE,
               use_labels = "label")

#... Adjustment Set ----

dag %>% 
  ggdag_adjustment_set()

#... D-seperated ----

# dag |> 
#   ggdag::ggdag_dseparated(controlling_for = "age")

# Depression  ----

# Diabetes Mellitus  ----

# High Blood Pressure  ----

# Myocardial Infarction  ----

# Stroke  ----

# Cancer  ----

# Heart Disease  ----

# COPD  ----

# Dementia  ----

# Infections  ----

# Pneumonia  ----
