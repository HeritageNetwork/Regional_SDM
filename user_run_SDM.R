# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# set project folder, db, species code, and species reaches filename for this run
rm(list=ls())
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- "D:/testing_SDM/Aquatic/species"
# Modeling database - provide full path
project_db <- "D:/testing_SDM/aqua_dev/databases/sdm_tracking_aqua_dev.sqlite"
# species code - from lkpSpecies in modelling database. This will be the new folder name in loc_model.
model_species <- "chrocumb"
# locations file (presence reaches). Provide full path; The file is copied to modeling folder and timestamped.
nm_presFile <- "D:/SDM/Tobacco/inputs/species/chrocumb/reach_data/chrocumb.csv"

## this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
## file anywhere on your computer, so you don't have to change the path)
## github branch to download
branch <- "Aquatic"
source("E:/git/aquatic/Regional_SDM/helper/get_scripts.R", local = TRUE)
## NOTE any messages, and download/place scripts manually if necessary

## loc_scripts should now be set; if it failed, 
## manually set loc_scripts path below if get_scripts fails
# loc_scripts <- "E:/git/aquatic/Regional_SDM/"

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_db","model_species","loc_scripts", "loc_model", "nm_presFile")])

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
# 5. huc_level: a numeric, 2-12. if used, will subset the background/prediction are of the model to 
#     the given HUC-level watershed(s) that presence reaches are within. Requires the 'huc12' column
#     to be present and populated in the presence reaches csv, env. vars csv, and all flowlines shapefile.

# RUN A NEW MODEL (ALL STEPS 1-5)
# If picking up from a previous run (after step 1), use Step 2-alt below
# update the function arguments below as necessary, and run the function
run_SDM(
  model_species = model_species, # species code in DB; new folder to create in loc_model if not existing
  loc_scripts = loc_scripts, 
  nm_presFile = nm_presFile,
  nm_db_file = project_db, 
  loc_model = loc_model,
  nm_envVars = "D:/SDM/Tobacco/env_vars/Tobacco_aqua/EnvVars.csv", # csv with comids, huc_12s, all variables
  nm_allflowlines = "D:/SDM/Tobacco/other_spatial/shp/aqua/VA_all_flowlines.shp", ### shapefile of all flowlines w/ comid, huc12 columns
  nm_refBoundaries = "D:/SDM/Tobacco/other_spatial/shp/aqua/StatesEast.shp", # background grey refernce lines in map
  nm_studyAreaExtent = "D:/SDM/Tobacco/other_spatial/shp/aqua/VA_HUC_predarea.shp", # outline black boundary line for study area in map
  nm_aquaArea = "D:/SDM/Tobacco/other_spatial/shp/aqua/VA_nhdarea_wb.shp", ### optional shapefile of all nhd 'area' types w/comid (for plotting model output)
  model_comments = "david's model test",
  metaData_comments = "bla bla",
  modeller = "David Bucklin",
  begin_step = "1",
  add_vars = NULL,
  remove_vars = NULL,
  huc_level = 2,
  prompt = FALSE
)

#############################################################################
#############################################################################
#############################################################################

# Step 2-alternate: run additional model, or pick up from previous model run

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
# or (if not provided) the location that was specified for the original model run.

# set project folder, species code, scripts for this run
project_db <- "D:/testing_SDM/aqua_dev/databases/sdm_tracking_aqua_dev.sqlite"
loc_model <- "D:/testing_SDM/aqua_dev/species"
model_species <- "chrocumb"
branch <- "aqua_dev"
source("E:/git/aquatic/Regional_SDM/helper/get_scripts.R", local = TRUE)
## NOTE any messages, and download/place scripts manually if necessary

# load function
setwd(loc_scripts)
source("helper/run_SDM.R")

# example pick-up a model run at step 2 (same presence/bkgd data, new model with different variables)
  # need to provide an input tableCode to nm_presFile 
  # to add/remove variables, begin at step 2
  # to just run new model, begin at step 3 (see next example)
run_SDM(
  begin_step = "2",
  model_species = "chrocumb",
  loc_model = loc_model,
  nm_presFile = "chrocumb_20180919_160305",
  model_comments = "Testing out model with removed variables.",
  remove_vars = "cbnfws"
)

# example pick-up a model run at step 3 (same presence/bkgd data, new model)
run_SDM(
  begin_step = "3",
  model_species = "chrocumb",
  loc_model = loc_model,
  nm_presFile = "chrocumb_20180919_160305",
  model_comments = "New model run using most recent settings."
)

# example pick-up a model run at step 4c (metadata/comment update)
  # if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4c",
  model_species = "alashete",
  loc_model = loc_model,
  model_rdata = "alashete_20180919_093614",
  metaData_comments = "This is an updated comment that will appear in the metadata PDF."
)
