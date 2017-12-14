# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# set project folder and species code for this run
project_folder <- "D:/SDM/Tobacco/"
model_species <- "ammohens"

# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "inputs/species/", model_species ,"/scripts")
# github branch to download
branch <- "dev"

# this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/git/Regional_SDM/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

# manually set loc_scripts path here if get_scripts fails
loc_scripts <- script_store

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts")])

# set wd and load function
setwd(loc_scripts)
source("run_SDM.R")

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
  loc_scripts = loc_scripts, 
  loc_spPoly = paste0(project_folder, "inputs/species/", model_species ,"/polygon_data"),
  nm_db_file = paste0(project_folder, "databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite"),
  loc_bkgPts = paste0(project_folder, "inputs/background/tobacco"), 
  nm_bkgPts = "tobacco_att",
  loc_envVars = paste0(project_folder, "env_vars/Tobacco"),
  loc_otherSpatial = paste0(project_folder, "other_spatial/shp"),
  nm_refBoundaries = "StatesVA",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_spPts = paste0(project_folder, "inputs/species/", model_species ,"/point_data"),
  loc_RDataOut = paste0(project_folder, "outputs/", model_species ,"/rdata"),
  loc_outRas = paste0(project_folder, "outputs/", model_species ,"/grids"),
  loc_outMetadata = paste0(project_folder, "outputs/", model_species ,"/metadata"),
  model_comments = "Updated sp. occurrences.",
  metaData_comments = "",
  modeller = "David Bucklin",
  add_vars = NULL,
  remove_vars = NULL,
  prompt = FALSE
)

#############################################################################
#############################################################################
#############################################################################

# Step 2-alt: run additional model, or pick up from previous model run

# if using add_vars or remove_vars for a new model run, start at step 2.

# if you want to run a new model with the same input data as the previous run, start at step 3.

# If picking up from a previously started run,
# provide the begin_step and path to loc_RDataOut. 
# When starting at script #4 or later, also provide the model rdata file 
# (stored in 'loc_RDataOut') to 'model_rdata', and any other 
# arguments that you wish to change from 
# the previous run (e.g., model_comments).
# 
# Note that you can manually update the scripts, if desired. The scripts
# will automatically be accessed from 'loc_scripts' location 
# that was specified for the original model run. 

# set project folder and species code for this run
project_folder <- "D:/SDM/Tobacco/"
model_species <- "glypmuhl"
# set model rdata, if starting at step 4 or later
# model_rdata <- "speciescode_20170101_123456"

# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "inputs/species/", model_species ,"/scripts")

# this downloads latest scripts from GitHub (you can save the 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/git/Regional_SDM/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

# manually set loc_scripts here if running step 1 seperately from step 2 (on different computers)
loc_scripts <- script_store

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts","model_rdata")])

# set wd and load function
setwd(loc_scripts)
source("run_SDM.R")

# UNCOMMENT BELOW
run_SDM(
  # loc_scripts = loc_scripts, # if script location is changed, make sure this is set
  begin_step = "5",
  loc_RDataOut = paste0(project_folder, "outputs/", model_species ,"/rdata"),
  model_rdata = model_rdata, # need to provide this if picking up after step 3, otherwise leave it out
  # model_comments = "Updated model comment.",
  # metaData_comments = "Updated metadata comment.",
  prompt = FALSE
)
