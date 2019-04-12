# File: user_run_SDM.r
# Purpose: Run a new, full SDM model (all steps)

library(here)
rm(list=ls())

# Step 1: Setting for the model run
# set project folder, db, species code, and species reaches filename for this run

# species code (from lkpSpecies in modelling database. This will be the new folder name containing inputs/ouptuts)
model_species <- "lasmsubv"

# loc_scripts is your repository. Make sure your git repository is set to correct branch
loc_scripts <- here()
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- here("_data", "species")
# Modeling database
nm_db_file <- here("_data", "databases", "SDM_lookupAndTracking.sqlite")
# locations file (presence reaches). Provide full path; File is copied to modeling folder and timestamped.
nm_presFile <- here("_data", "occurrence", paste0(model_species, ".csv"))
# map reference boundaries
nm_refBoundaries = here("_data","other_spatial","feature","US_States.shp") # background grey reference lines in map

# project overview - this appears in the first paragraph of the metadata
project_overview = "The following metadata describes the SDM for one species of 2,700 included in a Map of Biodiversity Irreplaceability (MoBI) in the continental U.S. developed by NatureServe and the Network of Natural Heritage Programs and funded by ESRI."

# model comment in database
model_comments = "2km sep distance"
# comment printed in PDF metadata
metaData_comments = ""
# your name
modeller = "Christopher Tracey"

# Name of background/envvars sqlite geodatabase, and base table name (2 length vector)
nm_bkg <- c(here("_data","env_vars","tabular", "background.sqlite"), "background_reaches")
# Name of background/envvars sqlite geodatabase, and base table name (2 length vector)
nm_huc12 <- c(here("_data","env_vars","tabular", "background.sqlite"), "range_huc12")
# name of aquatic areas shapefile (for mapping; optional) [Aquatic-only variable]
nm_aquaArea <- c(here("_data", "env_vars","tabular", "background.sqlite"), "nhdArea")

# numeric HUC level to sub-set project area [Aquatic-only variable]. NULL will auto-calculate the level where all presences are in a unique watershed at that level
huc_level <- NULL

# list non-standard variables to "add" to model run
add_vars = NULL
# list standard variables to remove from model run
remove_vars = NULL
# do you want to stop execution after each modeling step (script)?
prompt = FALSE

project_blurb = "Models developed for the MoBI project are intended to inform creation of a national map of biodiversity value, and we recommend additional refinement and review before these data are used for more targeted, species-specific decision making. In particular, many MoBI models would benefit from greater consideration of species data and environmental predictor inputs, a more thorough review by species experts, and iteration to address comments received."

# set wd and load function
setwd(loc_scripts)
source(here("helper", "run_SDM.R"))

##############
# End step 1 #
##############

# Step 2: execute a new model
# Usage: For a full, new model run, provide all paths/file names to arguments 'loc_scripts' THROUGH 'modeller'.
# RUN A NEW MODEL (ALL STEPS 1-5)
# If picking up from a previous run (after step 1), use Step 2-alt below
# update the function arguments below as necessary, and run the function
run_SDM(
  model_species = model_species, # species code in DB; new folder to create in loc_model if not existing
  loc_scripts = loc_scripts, 
  nm_presFile = nm_presFile,
  nm_db_file = nm_db_file, 
  loc_model = loc_model,
  nm_bkg = nm_bkg,
  nm_huc12 = nm_huc12,
  nm_aquaArea = nm_aquaArea, ### optional shapefile of all nhd 'area' types w/comid (for plotting model output)
  huc_level = huc_level,
  nm_refBoundaries = nm_refBoundaries, # background grey reference lines in map
  project_overview = project_overview,
  model_comments = model_comments,
  metaData_comments = metaData_comments,
  modeller = modeller,
  add_vars = add_vars,
  remove_vars = remove_vars,
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
model_species <- "uttepeni"
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
  begin_step = "3",
  model_species = "fuscburk",
  loc_model = loc_model,
  nm_presFile = "fuscburk_20190207_221919_prepped"
  #model_comments = "Testing out model with removed variables.",
  #remove_vars = "cbnfws"
)




# example pick-up a model run at step 5 (metadata create)
  # if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4",
  model_species = "pleucoll",
  loc_model = loc_model,
  model_rdata = "pleucoll_20190207_142005"
  #metaData_comments = "This is an updated comment that will appear in the metadata PDF."
)



# example pick-up a model run at step 4c (metadata/comment update)
# if starting at step 4 or later, must provide model run name to model_rdata
run_SDM(
  begin_step = "4c",
  model_species = "chrocumb",
  loc_model = loc_model,
  #rubric_default = rubric_default,
  model_rdata = "chrocumb_20190108_143402",
  model_comments = "Testing out model model comments.",
  metaData_comments = "This is an updated comment that will appear in the metadata PDF."
)

########## 
##########
##########

# TESTING / DEBUGGING ONLY
library(here)
rm(list=ls())
# Use the lines below for debugging (running line by line) for a certain script
# This loads the variables used in previous model run for the species, 
# so you need to have executed run_SDM in step 2 first.

# for scripts 1-3, run just the following 3 lines
model_species <- "villnebu"
load(here("_data","species",model_species,"runSDM_paths.Rdata"))
for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

# if debugging script 4 or later, also load the specific model output rdata file
model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))

