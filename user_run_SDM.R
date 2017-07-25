# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup an existing run executed using run_SDM.

# get function
setwd('D:/SDM/Tobacco-test/inputs/species/parahera/scripts')
source("run_SDM.R")

# execute a new model

# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.

# Optional arguments for all runs include:
# 1. begin_step: specify as the prefix of the step to begin with: one of ("1","2","3","4","4b","4c","5").
#     Defaults to "1", so not necessary for new runs.
# 2. prompt: if TRUE, the function will stop after each script, and ask if you want to continue. 
#     Defaults to FALSE.

run_SDM(
  loc_scripts = "D:/SDM/Tobacco-test/inputs/species/parahera/scripts" , 
  loc_spPoly = "D:/SDM/Tobacco-test/inputs/species/parahera/polygon_data",
  nm_db_file = "D:/SDM/Tobacco-test/databases/VA_Spp/SDM_lookupAndTracking_VA_20170712.sqlite",
  loc_bkgPts = "D:/SDM/Tobacco-test/inputs/background/tobacco", 
  nm_bkgPts = "tobacco_att",
  loc_envVars = "D:/SDM/Tobacco-test/env_vars/Tobacco",
  loc_otherSpatial = "D:/SDM/Tobacco-test/other_spatial/shp",
  nm_refBoundaries = "StatesVA",
  nm_studyAreaExtent = "sdmVA_pred_20170131",
  loc_spPts = "D:/SDM/Tobacco-test/inputs/species/parahera/point_data",
  loc_RDataOut = "D:/SDM/Tobacco-test/outputs/parahera/rdata",
  loc_outRas = "D:/SDM/Tobacco-test/outputs/parahera/grids",
  loc_outMetadata = "D:/SDM/Tobacco-test/outputs/parahera/metadata",
  model_comments = "NEW MODEL TEST runSDM fn.",
  modeller = "Your name",
  prompt = TRUE
)

# pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the path to loc_RDataOut. If after step
# 3, also provide the model rdata file (stored in loc_RDataOut) to 'model_rdata'.

# run_SDM(
#  loc_RDataOut = "D:/SDM/Tobacco-test/outputs/parahera/rdata",
#  model_rdata = "parahera_2017-07-24",
#  begin_step = "4",
#  prompt = FALSE
# )
