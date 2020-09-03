# File: user_run_SDM.r
# Purpose: Run a new, full SDM model (all steps)
library(checkpoint)
checkpoint("2020-04-22")

library(here)
rm(list=ls())
# 

# Step 1: Setting for the model run

# species code (from lkpSpecies in modelling database. This will be the new folder name containing inputs/ouptuts)
model_species <- "eriogyps"
# loc_scripts is your repository. Make sure your git repository is set to correct branch
loc_scripts <- here()
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- here("_data", "species")
# Modeling database
nm_db_file <- here("_data", "databases", "SDM_lookupAndTracking_NM.sqlite")
# locations file (presence reaches). Provide full path; File is copied to modeling folder and timestamped.
nm_presFile <- here("_data", "occurrence", paste0(model_species, ".shp"))
# env vars location [Terrestrial-only variable]
loc_envVars = here("_data","env_vars","raster","cropped")
# Name of background/envvars sqlite geodatabase, and base table name (2 length vector)
nm_bkgPts <- c(here("_data","env_vars","tabular", "background_NM.sqlite"), "background_pts")
# HUC spatial data set (shapefile) that is subsetted and used to define modeling area//range
nm_HUC_file <- here("_data","other_spatial","feature","HUC10.shp")
# map reference boundaries
nm_refBoundaries = here("_data","other_spatial","feature", "US_States.shp")  # background grey reference lines in map

# project overview - this appears in the first paragraph of the metadata
project_overview = "This model was developed for the U.S. Department of the Interior Bureau of Land Mangement."

# model comment in database
model_comments = ""

# comment printed in PDF metadata
metaData_comments = ""

# your name
modeller = "Tim Howard"

# list the algorithms to apply in an ensemble model
# options currently: "rf" (random forest), 
#                   "me" (maxent), 
#                   "xgb" (extreme gradient boosting), 
#                   [["gam" (generalized additive models) -- no not gam yet]]
ensemble_algos = c("rf", "me", "xgb")

# list non-standard variables to add to model run
add_vars = NULL
# list standard variables to exclude from model run
remove_vars = NULL
# do you want to stop execution after each modeling step (script)?
prompt = FALSE

project_blurb = ""

# set wd and load function
setwd(loc_scripts)
source(here("helper", "run_SDM.R"), local = FALSE)

##############
# End step 1 #
##############

# Step 2: execute a new model
# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.
# RUN A NEW MODEL (ALL STEPS 1-5)

run_SDM(
  model_species = model_species, # species code in DB; new folder to create in loc_model if not existing
  loc_scripts = loc_scripts, 
  nm_presFile = nm_presFile,
  nm_db_file = nm_db_file, 
  loc_model = loc_model,
  loc_envVars = loc_envVars,
  nm_bkgPts = nm_bkgPts,
  nm_HUC_file = nm_HUC_file,
  nm_refBoundaries = nm_refBoundaries, # background grey refernce lines in map
  project_overview = project_overview,
  model_comments = model_comments,
  metaData_comments = metaData_comments,
  modeller = modeller,
  ensemble_algos = ensemble_algos,
  add_vars = add_vars,
  remove_vars = remove_vars,
  #rubric_default = rubric_default,
  project_blurb = project_blurb,
  prompt = prompt
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

# or the location that was specified for the original model run. 
library(here)
rm(list=ls())

# set project folder and species code for this run
model_species <- "allimunz"
loc_model <- here("_data", "species")

# set wd and load function
loc_scripts <- here()
setwd(loc_scripts)
source(here("helper", "run_SDM.R"))

# example pick-up a model run at step 2 (same presence/bkgd data, new model with different variables)
  # need to provide an input tableCode to nm_presFile 
  # to add/remove variables, begin at step 2
  # to just run new model, begin at step 3 (see next example)
run_SDM(
  begin_step = "2",
  model_species = "eriogyps",
  loc_model = loc_model,
  loc_scripts = loc_scripts
)



# example pick-up a model run at step 5 (metadata create)
  # if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4",
  model_species = "geumpeck",
  loc_model = loc_model,
  model_rdata = "geumpeck_20190409_155720"
)



# example pick-up a model run at step 4c (metadata/comment update)
# if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4b",
  model_species = "amsothar",
  loc_model = loc_model,
  loc_scripts = loc_scripts,
  model_rdata = max(list.files(here("_data","species",model_species,"outputs","rdata")))
)

########## 
##########
##########

# TESTING / DEBUGGING ONLY
library(checkpoint)
checkpoint("2020-04-22")

library(here)
rm(list=ls())
# Use the lines below for debugging (running line by line) for a certain script
# This loads the variables used in previous model run for the species, 
# so you need to have started a run_SDM() run in step 2 first.

# for scripts 1-3, run just the following 3 lines

model_species <- "eriogyps"

load(here("_data","species",model_species,"runSDM_paths_most_recent.Rdata"))
# if you want an earlier run, enter it and load it here:
#load(here("_data","species",model_species,"runSDM_paths_amsothar_20200508_171152.Rdata"))
for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

# if debugging script 4 or later, also load the specific model output rdata file
#model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
model_rdata <- paste0(fn_args$modelrun_meta_data$model_run_name, ".Rdata")
load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))


#######
###  loop it
######

x <- list.files(path = here("_data","occurrence"), pattern = "*.shp$")
sppVec <- sub(".shp","",x)

sppVec

for(sv in 1:length(sppVec))
  {
    run_SDM(
      model_species = sppVec[[sv]],
      loc_scripts = here(), 
      nm_presFile = here("_data", "occurrence", paste0(sppVec[[sv]], ".shp")),
      nm_db_file = here("_data", "databases", "SDM_lookupAndTracking.sqlite"), 
      loc_model = here("_data", "species"),
      loc_envVars = here("_data","env_vars","raster"),
      nm_bkgPts = c(here("_data","env_vars","tabular", "background_CA.sqlite"), "background_pts"),
      nm_HUC_file = here("_data","other_spatial","feature","HUC10.shp"),
      nm_refBoundaries = here("_data","other_spatial","feature", "US_States.shp"), 
      project_overview = "The following metadata describes the SDM for one species of 2,700 included in a Map of Biodiversity Importance (MoBI) in the continental U.S. developed by NatureServe and the Network of Natural Heritage Programs and funded by ESRI.",
      model_comments = "",
      metaData_comments = "",
      modeller = "Tim Howard",
      add_vars = NULL,
      remove_vars = NULL,
      project_blurb = "Models developed for the MoBI project are intended to inform creation of a national map of biodiversity value, and we recommend additional refinement and review before these data are used for more targeted, species-specific decision making. In particular, many MoBI models would benefit from greater consideration of species data and environmental predictor inputs, a more thorough review by species experts, and iteration to address comments received.",
      prompt = FALSE
    )
  }

