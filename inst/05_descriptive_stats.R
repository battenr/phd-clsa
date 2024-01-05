# Title: Descriptive statistics

# Description: Descriptive statistics for the following: 
# - Overall
# - Covariates
# - Stratified by BZD use 
# - Outcomes

# Setup ----

#... Libraries ----

library(tidyverse)
library(survey)
library(officer)
library(flextable)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

# Descriptive Stats ---

# First we need to load our data 

# Data 

load("data/analysis_dataset.Rdata")

# Survey Design ----

design.infl <- svydesign(data = df, 
                         weights= ~wghts_inflation, 
                         strata = ~geostrata,
                         #fpc = ~strata_total,
                         ids = ~1,
                         nest = TRUE)

# Descriptive Analysis ----

#... BZDs ----

# svy_count("bzd", design = design.infl, data = df)
# 
# 1311/(1311+28786)

# Categorical Variables ----

cat_var <- df |> 
  dplyr::select(
    
    # Selecting all except for participant ID, age (as a number) and weights/geostrata
    
    -c(entity_id,
       age_nmbr, 
       starts_with("wghts"), 
       geostrata
       )
    ) |> 
  names()

count_cat = cat_var |> 
  map(
    \(x)svy_count(x, design = design.infl, data = df)
  ) 

names(count_cat) <- cat_var

list2env(count_cat, envir = .GlobalEnv)


# Create a new Word document
doc <- read_docx()

# Loop through each named data frame and add it to the Word doc
for(name in names(count_cat)) {
  doc <- doc %>%
    body_add_flextable(flextable::qflextable(count_cat[[name]])) %>%
    body_add_break()
}

# Stratified by BZD Use ----

count_cat = cat_var |> 
  map(
    \(x)svy_count(x, design = design.infl, data = df)
  ) 








# Save the document
print(doc, target = "output/Categorical Variables.docx")

# Continuous Variables ----

# Stratified by BZD Use ----

#... Categorical ----

#... Continuous Variables ----
