# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# set project folder and species code for this run
project_folder <- "E:/SDM/Aquatic"
model_species <- "lasmcoml"
spReaches <- "alashete_test1"


# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "/species/", model_species ,"/inputs/scripts")
# github branch to download
branch <- "aqua_dev"

# this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/SDM/Aquatic/scripts/Regional_SDM/helper/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

# manually set loc_scripts path here if get_scripts fails
loc_scripts <- script_store
loc_scripts <- "E:/git/aquatic/Regional_SDM"

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts", "spReaches")])

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
#     to be in presence reaches csv, env. vars csv, and all flowlines shapefile.

# RUN A NEW MODEL (ALL STEPS 1-5)
# If picking up from a previous run (after step 1), use Step 2-alt below
# update the function arguments below as necessary, and run the function
run_SDM(
  loc_scripts = loc_scripts, 
  loc_spReaches = paste0(project_folder, "/species/", model_species , "/inputs/presence"), ### name of file is speciescode.csv
  nm_spReaches = spReaches,
  nm_db_file = paste0(project_folder, "/databases/sdm_tracking_dum.sqlite"),
  loc_modelIn = paste0(project_folder, "/species/", model_species , "/inputs/model_input"),
  loc_envVars = paste0(project_folder, "/env_vars/tabular"), ### all reaches with env. var. attributes (name of file is EnvVars.csv)
  loc_otherSpatial = paste0(project_folder, "/other_spatial/feature"),
  nm_allflowlines = "VA_all_flowlines", ### shapefile of all flowlines w/ comid, huc12 columns
  nm_refBoundaries = "StatesEast", # background refernce lines in map
  nm_studyAreaExtent = "VA_HUC_predarea", # outline boundary for study area in map
  nm_aquaArea = "VA_nhdarea_wb", ### optional shapefile of all nhd 'area' types w/comid (for plotting model output)
  loc_modelOut = paste0(project_folder, "/species/", model_species, "/outputs"), # replaces seperate metadata, rdata, shapefiles variables
  model_comments = "test run new structure",
  metaData_comments = "bla bla bla",
  modeller = "David Bucklin",
  begin_step = "1",
  add_vars = NULL,
  remove_vars = NULL,
  huc_level = 2,
  prompt = TRUE

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

# same prep steps as above

# set project folder and species code for this run
project_folder <- "D:/testing_SDM/dum"
model_species <- "alashete"
# set model rdata, if starting at step 4 or later
model_rdata <- "alashete_20180803_123306"

# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "/species/", model_species ,"/inputs/scripts")
# github branch to download
branch <- "aqua_dev"

# this downloads latest scripts from GitHub (you can save the 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/SDM/Aquatic/scripts/Regional_SDM/helper/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

# manually set loc_scripts here if running step 1 seperately from step 2 (on different computers)
loc_scripts <- script_store

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts","model_rdata")])

# set wd and load function
setwd(loc_scripts)
source("helper/run_SDM.R")

# pick-up a model run after step 1 (uncomment below)
run_SDM(
 begin_step = "4",
 loc_modelOut = paste0(project_folder, "/species/", model_species, "/outputs"),
 model_rdata = model_rdata, # need to provide this if picking up after step 3, otherwise leave it out
 model_comments = "none",
 prompt = FALSE
)

