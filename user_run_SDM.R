# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.

# After running, save this file in the species' 'loc_scripts' folder

# set project folder and species code for this run
project_folder <- "D:/SDM/Tobacco/"

model_species <- "iofluv"

# Step 1: retrieve latest function/scripts from GitHub
loc_scripts <- paste0(project_folder, "inputs/species/",model_species,"/scripts")

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
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts")])

# set wd and load function
setwd(loc_scripts)
source("run_SDM.R")

run_SDM(
  loc_scripts = loc_scripts, 
  loc_spReaches = paste0(project_folder, "inputs/species/", model_species , "/reach_data"), ### name of file is speciescode.csv
  nm_db_file = paste0(project_folder, "databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite"),
  loc_bkgReach = paste0(project_folder, "inputs/species/", model_species , "/background"),
  loc_envVars = paste0(project_folder, "env_vars/Tobacco_aqua"), # all reaches with env var attributes (EnvVars.csv)
  loc_otherSpatial = paste0(project_folder, "other_spatial/shp/aqua"),
  nm_allflowlines = "VA_all_flowlines", ### shapefile of all flowlines w/ comid, huc12 columns
  nm_refBoundaries = "StatesEast",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_RDataOut = paste0(project_folder, "outputs/", model_species , "/rdata"),
  loc_outVector = paste0(project_folder, "outputs/", model_species , "/shapefiles"),
  loc_outMetadata = paste0(project_folder, "outputs/", model_species , "/metadata"),
  model_comments = "",
  metaData_comments = "",
  modeller = "David Bucklin",
  begin_step = "1",
  prompt = FALSE
)

# Step 2-alt: pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the path to loc_RDataOut. If after script
# step #3, also provide the model rdata file (stored in loc_RDataOut) to 'model_rdata'.
# Note that you can manually update your scripts, if desired. The scripts
# will be accessed from 'loc_scripts' as specified in the original model run.

run_SDM(
 begin_step = "2",
 model_comments = "implemented correlated variable removal",
 metaData_comments = "",
 loc_RDataOut = paste0(project_folder, "outputs/", model_species , "/rdata"),
 # model_rdata = paste0(model_species , "_20170928_123026"), # need to provide this if picking up after step 3, otherwise leave it out
 prompt = FALSE
)
