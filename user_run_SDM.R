# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

library(here)
rm(list=ls())

# Step 1: Setting for the model run
# set project folder, db, species code, and species reaches filename for this run

# species code (from lkpSpecies in modelling database. This will be the new folder name containing inputs/ouptuts)
model_species <- "bombferv"
# loc_scripts is your repository. Make sure your git repository is set to correct branch
loc_scripts <- here()
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- here("_data", "species")
# Modeling database
nm_db_file <- here("_data", "databases", "SDM_lookupAndTracking.sqlite")
# locations file (presence reaches). Provide full path; File is copied to modeling folder and timestamped.
nm_presFile <- here("_data", "occurrence", paste0(model_species, ".shp"))
# env vars location [Terrestrial-only variable]
loc_envVars = here("_data","env_vars","raster")
# bkg points [Terrestrial-only variable]
nm_bkgPts = here("_data","env_vars","background","va_att.shp")
# map reference boundaries
nm_refBoundaries = here("_data","other_spatial","feature","StatesEast.shp") # background grey refernce lines in map
# map project boundary
nm_studyAreaExtent = here("_data","other_spatial","feature","sdmVA_pred_20170131.shp") # outline black boundary line for study area in map
# model comment in database
model_comments = "testing master"
# comment printed in PDF metadata
metaData_comments = "bla bla"
# your name
modeller = "David Bucklin"

# list non-standard variables to "add" to model run
add_vars = NULL
# list standard variables to remove from model run
remove_vars = NULL
# do you want to stop execution after each modeling step (script)?
prompt = TRUE

# set wd and load function
setwd(loc_scripts)
source(here("helper", "run_SDM.R"))

##############
# End step 1 #
##############

# Step 2: execute a new model
# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.
# RUN A NEW MODEL (ALL STEPS 1-5)
# If picking up from a previous run (after step 1), use Step 2-alt below
# update the function arguments below as necessary, and run the function

run_SDM(
  begin_step = "1",
  model_species = model_species, # species code in DB; new folder to create in loc_model if not existing
  loc_scripts = loc_scripts, 
  nm_presFile = nm_presFile,
  nm_db_file = nm_db_file, 
  loc_model = loc_model,
  loc_envVars = loc_envVars,
  nm_bkgPts = nm_bkgPts,
  nm_refBoundaries = nm_refBoundaries, # background grey refernce lines in map
  nm_studyAreaExtent = nm_studyAreaExtent, # outline black boundary line for study area in map
  model_comments = model_comments,
  metaData_comments = metaData_comments,
  modeller = modeller,
  add_vars = add_vars,
  remove_vars = remove_vars,
  prompt = prompt
)

#############################################################################
#############################################################################
#############################################################################

# Step 2-alt: run additional model, or pick up from previous model run

# if using add_vars or remove_vars for a new model run, start at step 2.

# if you want to run a new model with the same input data as a previous run, start at step 3.

# If picking up from a previously started run, always
# provide the begin_step, model_species, and loc_model.
# When starting at script #4 or later, also provide the name of the 
# model rdata file to 'model_rdata'. 
# You can also include any other arguments that you wish to change from 
# the previous run (e.g., model_comments or metaData_comments).
# 
# Note that you can manually update the scripts, if desired. 
# The scripts will automatically be accessed from 'loc_scripts' (if provided) 
# or the location that was specified for the original model run. 
library(here)
rm(list=ls())

# set project folder and species code for this run
model_species <- "micrmont"
loc_model <- here("_data", "species")

# set wd and load function
loc_scripts <- here()
setwd(loc_scripts)
source(here("helper", "run_SDM.R"))

# example pick-up a model run at step 3 (new model, same presence/bkgd data)
  # if starting at step 2/3, provide an input tableCode to nm_presFile 
  # to add/remove vars, begin at step 2
  # to just run new model, begin at step 3
run_SDM(
  begin_step = "2",
  model_species = "micrmont",
  loc_model = loc_model,
  nm_presFile = "micrmont_20181015_123515",
  remove_vars = "elevx10"
)

# example pick-up a model run at step 4c (metadata/comment update)
  # if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4c",
  model_species = "micrmont",
  loc_model = loc_model,
  model_rdata = "micrmont_20181017_124730",
  metaData_comments = "UPDATED METADATA COMMENT"
)


########## 
##########
##########

# TESTING / DEBUGGING ONLY
library(here)
rm(list=ls())
# Use the lines below for debugging (running line by line) for a certain script
# This loads the variables used in previous model run for the species, 
# so you need to have executed run_SDM in step 2 first.

# for scripts 1-3, run just the following 3 lines
model_species <- "bombferv"
load(here("_data","species",model_species,"runSDM_paths.Rdata"))
for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

# if debugging script 4 or later, also load the specific model output rdata file
model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))

