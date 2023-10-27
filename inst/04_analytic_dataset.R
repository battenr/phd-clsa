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
