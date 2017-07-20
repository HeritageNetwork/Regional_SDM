# File: user_run_SDM.r
# Purpose: Run a full SDM model, or pickup a 

# get function
source("run_SDM.R")

# execute a new model

# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.

# Optional arguments for all runs include:
# 1. begin_step: specify as the prefix of the step to begin with: one of ("1","2","3","4","4b","4c","5").
#     Defaults to "1", so not necessary for new runs.
# 2. model_rdata: when beginning after step 3, you need to specify the Rdata file name (no file extension) 
#     for the model previously created. Will be looked for in the 'loc_RDataOut' folder.
# 3. prompt: if TRUE, the function will stop after each script, and ask if you want to continue. 
#     Defaults to FALSE.

run_SDM(
  loc_scripts, 
  loc_spPoly,
  nm_db_file,
  loc_bkgPts, 
  nm_bkgPts,
  loc_envVars,
  loc_otherSpatial,
  nm_refBoundaries,
  nm_studyAreaExtent,
  loc_spPts,
  loc_RDataOut,
  loc_outRas,
  loc_outMetadata,
  model_comments,
  modeller,
  prompt = FALSE
)

# pick up from previous model run (uncomment below)

# If picking up from a previous run, provide the full file location to the saved rdata file (no file extension)
# holding these paths. For new runs, this file is automatically saved as "runSDM_paths" in the 
# 'loc_RDataOut' folder of the original run.

# run_SDM(
#   paths_rdata = "PATH:/runSDM_paths",
#   begin_step = "1",
#   model_rdata = NULL,
#   prompt = FALSE
# )