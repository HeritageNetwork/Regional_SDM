# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.

# After running, save this file in the species' 'loc_scripts' folder

# Step 1: retrieve latest function/scripts from GitHub
loc_scripts <- "D:/SDM/Tobacco/inputs/species/chrotenn/scripts"

# this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/git/aquatic/Regional_SDM/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

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

# manually set loc_scripts here if running step 1 seperately from step 2 (on different computers)
loc_scripts <- script_store

# remove everything but loc_scripts
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% "loc_scripts"])

# during testing, just set to local git repo
loc_scripts <- "E:/git/aquatic/Regional_SDM"

# set wd and load function
setwd(loc_scripts)
source("run_SDM.R")

run_SDM(
  loc_scripts = loc_scripts, 
  loc_spReaches = "D:/SDM/Tobacco/inputs/species/chrotenn/reach_data",
  nm_db_file = "D:/SDM/Tobacco/databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite",
  loc_bkgReach = "D:/SDM/Tobacco/inputs/species/chrotenn/background", 
  loc_envVars = "D:/SDM/Tobacco/env_vars/Tobacco_aqua",
  loc_otherSpatial = "D:/SDM/Tobacco/other_spatial/shp/aqua",
  nm_allflowlines = "all_VA_flowlines_wHUC12",
  nm_refBoundaries = "StatesEast",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_RDataOut = "D:/SDM/Tobacco/outputs/chrotenn/rdata",
  loc_outVector = "D:/SDM/Tobacco/outputs/chrotenn/shapefiles",
  loc_outMetadata = "D:/SDM/Tobacco/outputs/chrotenn/metadata",
  model_comments = "Aqua model testing..., minimal env vars",
  metaData_comments = "This comment will be in the final PDF.",
  modeller = "David Bucklin",
  prompt = FALSE
)

# Step 2-alt: pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the path to loc_RDataOut. If after script
# step #3, also provide the model rdata file (stored in loc_RDataOut) to 'model_rdata'.
# Note that you can manually update your scripts, if desired The scripts
# will be accessed from 'loc_scripts' as specified in the original model run.

run_SDM(
  begin_step = "3",
  loc_RDataOut = "D:/SDM/Tobacco/outputs/chrotenn/rdata",
  # model_rdata = "chrotenn_20170817_152419", # need to provide this if picking up after step 3, otherwise leave it out
  prompt = TRUE
)
