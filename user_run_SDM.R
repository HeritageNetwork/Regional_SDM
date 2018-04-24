# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.
# After running a full model, save this file in the species' 'loc_scripts' folder

# Step 1: Download updated scripts from GitHub repository
# Usage: to put the latest modeling scripts in a new folder created in 'loc_scripts' (set below),
# which are used for new modeling runs

# set project folder and species code for this run
project_folder <- "E:/SDM/Aquatic"
model_species <- "alasvari"

# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "/inputs/species/", model_species ,"/scripts")
# github branch to download
branch <- "Aquatic"

# this downloads latest scripts from GitHub (you can save this 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/SDM/Aquatic/scripts/Regional_SDM/get_scripts.R", local = TRUE)
# NOTE any messages, and download/place scripts manually if necessary

# manually set loc_scripts path here if get_scripts fails
loc_scripts <- script_store

# remove everything but necessary variables
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("project_folder","model_species","loc_scripts")])

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
  loc_scripts = loc_scripts, 
  loc_spReaches = paste0(project_folder, "/inputs/species/", model_species , "/reach_data"), ### name of file is speciescode.csv
  nm_db_file = paste0(project_folder, "/databases/SDM_lookupAndTracking_new.sqlite"),
  loc_bkgReach = paste0(project_folder, "/inputs/species/", model_species , "/background"),
  loc_envVars = paste0(project_folder, "/env_vars"), ### all reaches with env. var. attributes (name of file is EnvVars.csv)
  loc_otherSpatial = paste0(project_folder, "/other_spatial"),
  nm_allflowlines = "PA_all_flowlines", ### shapefile of all flowlines w/ comid, huc12 columns
  nm_refBoundaries = "StatesEast",
  nm_studyAreaExtent = "PA_HUC_predarea", #"PA_HUC_predarea"
  nm_aquaArea = "PA_nhdarea_wb", ### optional shapefile of all nhd 'area' types w/comid (for plotting model output)
  loc_RDataOut = paste0(project_folder, "/outputs/", model_species , "/rdata"),
  loc_outVector = paste0(project_folder, "/outputs/", model_species , "/shapefiles"),
  loc_outMetadata = paste0(project_folder, "/outputs/", model_species , "/metadata"),
  model_comments = "",
  metaData_comments = "",
  modeller = "Christopher Tracey",
  begin_step = "1",
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

# same prep steps as above
library(RSQLite)
db <- dbConnect(SQLite(),dbname="E:/SDM/Aquatic/databases/SDM_lookupAndTracking_new.sqlite")
biglist <- dbGetQuery(db, "SELECT code from lkpSpecies where modtype = 'A';")$CODE
biglist <- biglist[!biglist %in% c("lampradi")]
for (ms in biglist) {
print(ms)
# set project folder and species code for this run
project_folder <- "E:/SDM/Aquatic"
model_species <- ms
# set model rdata, if starting at step 4 or later
 model_rdata <- "alasvari_20180207_124154"

# path where you want to save model run scripts
loc_scripts <- paste0(project_folder, "/inputs/species/", model_species ,"/scripts")
# github branch to download
branch <- "Aquatic"

# this downloads latest scripts from GitHub (you can save the 'get_scripts.R' 
# file anywhere on your computer, so you don't have to change the path)
source("E:/SDM/Aquatic/scripts/Regional_SDM/get_scripts.R", local = TRUE)
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
 begin_step = "5",
 loc_RDataOut = paste0(project_folder, "/outputs/", model_species , "/rdata"),
 model_rdata = model_rdata, # need to provide this if picking up after step 3, otherwise leave it out
 nm_aquaArea = "PA_nhdarea_wb",
 nm_refBoundaries = "StatesEast",
 model_comments = "variable set <1% missing by variable. New sampling method (75% of all reaches), affects thresholds.",
 prompt = FALSE
)
}

