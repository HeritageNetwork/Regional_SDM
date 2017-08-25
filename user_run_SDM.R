# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# path where you want to save model run scripts
loc_scripts <- "D:/SDM/Tobacco/inputs/species/parahera/scripts"

# this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/git/Regional_SDM/get_scripts.R", local = TRUE)
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

# set wd and load function
setwd(loc_scripts)
source("run_SDM.R")


# RUN A NEW MODEL (ALL STEPS 1-5)
# If picking up from a previous run (after step 1), use Step 2-alt below
# update the function arguments below as necessary, and run the function
run_SDM(
  begin_step = "1",
  loc_scripts = loc_scripts, 
  loc_spPoly = "D:/SDM/Tobacco/inputs/species/parahera/polygon_data",
  nm_db_file = "D:/SDM/Tobacco/databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite",
  loc_bkgPts = "D:/SDM/Tobacco/inputs/background/tobacco", 
  nm_bkgPts = "tobacco_att",
  loc_envVars = "D:/SDM/Tobacco/env_vars/Tobacco",
  loc_otherSpatial = "D:/SDM/Tobacco/other_spatial/shp",
  nm_refBoundaries = "StatesVA",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_spPts = "D:/SDM/Tobacco/inputs/species/parahera/point_data",
  loc_RDataOut = "D:/SDM/Tobacco/outputs/parahera/rdata",
  loc_outRas = "D:/SDM/Tobacco/outputs/parahera/grids",
  loc_outMetadata = "D:/SDM/Tobacco/outputs/parahera/metadata",
  model_comments = "This is an internal comment.",
  metaData_comments = "This comment will appear in the output PDF.",
  modeller = "David Bucklin",
  add_vars = NULL,
  remove_vars = c("nlcdopn100", "nlcdopn10", "nlcdopn1"),
  prompt = FALSE
)

# Step 2-alt: pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the begin_step and path to loc_RDataOut. 
# If after script step #3, also provide the model rdata file (stored in loc_RDataOut)
# to 'model_rdata'.
# 
# Note that you can manually update the scripts, if desired. The scripts
# will automatically be accessed from 'loc_scripts' location 
# that was specified for the original model run.

# run_SDM(
#   begin_step = "4",
#   loc_RDataOut = "D:/SDM/Tobacco/outputs/parahera/rdata",
#   # model_rdata = "parahera_20170816_152627", # need to provide this if picking up after step 3, otherwise leave it out
#   prompt = FALSE
# )
