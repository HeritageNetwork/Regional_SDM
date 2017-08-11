# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.

# After running, save this file in the species' 'loc_scripts' folder
library(git2r)

# Step 1: retrieve latest function/scripts from GitHub
loc_scripts <- "D:/SDM/Tobacco/inputs/species/parahera/scripts"

# download from GitHub, latest scripts
script_store <- paste0(loc_scripts, "/Regional_SDM_", Sys.Date())
if (!dir.exists(script_store)) {
  try(suppressMessages(git_repo <- git2r::clone("https://github.com/PNHP/Regional_SDM.git",
                                              branch = "aqua_dev", local_path = script_store)), silent = TRUE)
  if (exists("git_repo")) {
    message("Scripts downloaded. Ready to run.")
  } else {
    dir.create(script_store)
    message(paste0("Couldn't download latest scripts. \nNew folder '",
                   script_store, "' created. \nPlace latest scripts in there."))
  }
} else {
  try({
    git_repo <- git2r::repository(script_store)
    git_pull <- git2r::pull(git_repo)
  })
  if (exists("git_pull") & (git_pull@up_to_date || git_pull@fast_forward)) {
    message("Scripts up-to-date. Ready to run.")
  } else {
    message(paste0("Couldn't download latest scripts.\n
                   Make sure latest scripts are in folder '",
                   script_store, "'"))
  }
}
# NOTE any messages, and download/place scripts manually if necessary

# this sets script dir. and remove all objects except loc_scripts
loc_scripts <- script_store
rm(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% "loc_scripts"])

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

run_SDM(
  loc_scripts = loc_scripts, 
  loc_spReaches = "D:/SDM/Tobacco/inputs/species/parahera/polygon_data",
  nm_db_file = "D:/SDM/Tobacco/databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite",
  loc_bkgReach = "D:/SDM/Tobacco/inputs/background/tobacco", 
  nm_bkgPts = "tobacco_att",
  loc_envVars = "D:/SDM/Tobacco/env_vars/Tobacco",
  loc_otherSpatial = "D:/SDM/Tobacco/other_spatial/shp",
  nm_refBoundaries = "StatesVA",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_spPts = "D:/SDM/Tobacco/inputs/species/parahera/point_data",
  loc_RDataOut = "D:/SDM/Tobacco/outputs/parahera/rdata",
  loc_outVector = "D:/SDM/Tobacco/outputs/parahera/grids",
  loc_outMetadata = "D:/SDM/Tobacco/outputs/parahera/metadata",
  model_comments = "Internal comment for modeler, stored in tblModelRuns.",
  metaData_comments = "This comment will be in the final PDF.",
  modeller = "David Bucklin",
  prompt = TRUE
)

# Step 2-alt: pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the path to loc_RDataOut. If after script
# step #3, also provide the model rdata file (stored in loc_RDataOut) to 'model_rdata'.
# Note that you can manually update your scripts, if desired The scripts
# will be accessed from 'loc_scripts' as specified in the original model run.

# run_SDM(
#   begin_step = "4",
#   loc_RDataOut = "D:/SDM/Tobacco/outputs/parahera/rdata",
#   model_rdata = "parahera_20170802_120026", # need to provide this if picking up after step 3, otherwise leave it out
#   prompt = TRUE
# )
