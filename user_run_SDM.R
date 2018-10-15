# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# set project folder, db, species code, and species reaches filename for this run
rm(list=ls())
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- "E:/git/dnbucklin/Regional_SDM/_data/species"
# Modeling database
nm_db_file <- "E:/git/dnbucklin/Regional_SDM/_data/databases/sdm_tracking_dev_all.sqlite"
# species code (from lkpSpecies in modelling database. This will be the new folder name in loc_model.)
model_species <- "micrmont"
# locations file (presence reaches). Provide full path; File is copied to modeling folder and timestamped.
nm_presFile <- "D:/SDM/Tobacco/inputs/species/micrmont/polygon_data/micrmont.shp"

### USE THIS SECTION IS IF YOU WANT A NEW SCRIPT REPO FOR THIS RUN
## this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
## file anywhere on your computer, so you don't have to change the path)
## github branch to download

# branch <- "master"
# source("E:/git/dnbucklin/Regional_SDM/helper/get_scripts.R", local = TRUE)

### NOTE any messages, and download/place scripts manually if necessary
######
######

### OTHERWISE, for testing, or if get_scripts failed, just set loc_scripts below
loc_scripts <- "E:/git/dnbucklin/Regional_SDM/"

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("nm_db_file","model_species","loc_scripts", "loc_model", "nm_presFile")])

# set wd and load function
setwd(loc_scripts)
source("helper/run_SDM.R")

##############
# End step 1 #
##############

# Step 2: execute a new model
# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.

# Optional arguments for all runs include:
# 1. begin_step: specify as the prefix of the step to begin with: one of ("1","2","3","4","4b","4c","5").
#     Defaults to "1", so not necessary to specify for new runs.
# 2. prompt: if TRUE, the function will stop after each script, and ask if you want to continue. 
#     Defaults to FALSE.
# 3. add_vars: variables that are not part of the standard set for this species, which you wish to 
#     include in the model run.
# 4. remove_vars: variables that are part of the standard set for this species, which you wish to
#     remove from the model run.

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
  loc_envVars = "D:/SDM/Tobacco/env_vars/Tobacco",
  nm_bkgPts = "D:/SDM/Tobacco/inputs/background/tobacco/tobacco_att.shp",
  nm_refBoundaries = "D:/SDM/Tobacco/other_spatial/shp/StatesEast.shp", # background grey refernce lines in map
  nm_studyAreaExtent = "D:/SDM/Tobacco/other_spatial/shp/sdmVA_pred_20170131.shp", # outline black boundary line for study area in map
  model_comments = "testing master (terrestrial)",
  metaData_comments = "bla bla",
  modeller = "David Bucklin",
  add_vars = NULL,
  remove_vars = NULL,
  prompt = TRUE
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

# set project folder and species code for this run
loc_model <- "E:/git/dnbucklin/Regional_SDM/_data/species"
nm_db_file <- "E:/git/dnbucklin/Regional_SDM/_data/databases/sdm_tracking_dev_all.sqlite"

# set wd and load function
loc_scripts <- "E:/git/dnbucklin/Regional_SDM/"
setwd(loc_scripts)
source("helper/run_SDM.R")

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
  begin_step = "4",
  model_species = "micrmont",
  loc_model = loc_model,
  model_rdata = "micrmont_20181015_124415",
  metaData_comments = "UPDATED METADATA COMMENT"
)
