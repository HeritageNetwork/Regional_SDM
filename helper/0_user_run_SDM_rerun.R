# File: user_run_SDM_rerun.r
# Purpose: Pick up from an existing SDM run using run_SDM()

# If picking up from a previously started run, always
# provide the begin_step, model_species, and loc_model.
# When starting at script #4 or later, also provide the name of the 
# model rdata file to 'model_rdata'. 
# You can also include any other arguments that you wish to change from 
# the previous run (e.g., model_comments or metaData_comments).
# 
# if using add_vars or remove_vars for a new model run, start at step 2.
#
# if you want to run a new model with the same input data as a previous run, start at step 3.
#
# if you want to update metadata, start at 4c (for a new comment) or 5 (just re-run metadata)
#
# Note that you can manually update the scripts, if desired. 
# The scripts will automatically be accessed from 'loc_scripts' (if provided) 
# or the location that was specified for the original model run (if not provided). 

rm(list=ls())
setwd("..")
library(here)

# set wd and load function
loc_scripts <- here()
setwd(loc_scripts)
source(here("helper", "run_SDM.R"))


# These are variables necessary for all picked-up runs
model_species <- "micrmont"
loc_model <- here("_data", "species")
begin_step <- "3"
# For picked-up runs after step 3, a model_run_name from tblModelResults is required.
model_rdata <- "micrmont_20181017_124730"


# a minimal run example; This re-uses function arguments for the latest run for the species
run_SDM(
  begin_step = begin_step,
  model_species = model_species,
  loc_model = loc_model
)


# Any variable can be added to the call to assign an updated value:
args(run_SDM)
