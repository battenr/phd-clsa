# Title: DAGs

# Description: Directed acyclic graphs for outcomes (DAGs)

# Setup ----

#... Libraries ----

library(tidyverse)

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

