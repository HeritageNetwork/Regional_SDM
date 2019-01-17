# File: user_run_SDM.r
# Purpose: Run a new, full SDM model (all steps)

library(here)
rm(list=ls())

# Step 1: Setting for the model run

# species code (from lkpSpecies in modelling database. This will be the new folder name containing inputs/ouptuts)
model_species <- "anaxexsu"
# loc_scripts is your repository. Make sure your git repository is set to correct branch
loc_scripts <- here()
# The main modelling folder for inputs/outputs. All sub-folders are created during the model run (when starting with step 1)
loc_model <- here("_data", "species")
# Modeling database
nm_db_file <- here("_data", "databases", "SDM_lookupAndTracking.sqlite")
# locations file (presence reaches). Provide full path; File is copied to modeling folder and timestamped.
nm_presFile <- here("_data", "occurrence", paste0(model_species, ".shp"))
# env vars location [Terrestrial-only variable]
loc_envVars = here("_data","env_vars","raster", "ras")
# Name of background/envvars sqlite geodatabase, and base table name (2 length vector)
nm_bkgPts <- c(here("_data","env_vars","tabular", "background.sqlite"), "background_pts")

# HUC spatial data set (shapefile) that is subsetted and used to define modeling area//range
nm_HUC_file <- here("_data","other_sp","HUC10.shp")
# map reference boundaries
nm_refBoundaries = here("_data","other_spatial","feature", "US_States.shp")  # background grey reference lines in map

# model comment in database
model_comments = "custom model comments"
# comment printed in PDF metadata
metaData_comments = "custom metadata comments"
# your name
modeller = "Tim Howard"

# list non-standard variables to add to model run
add_vars = NULL
# list standard variables to exclude from model run
remove_vars = NULL
# do you want to stop execution after each modeling step (script)?
prompt = TRUE

# default values for Model Use rubric
# order should be "spdata_dataqual,spdata_abs,spdata_eval,envvar_relevance,envvar_align,process_algo,process_sens,process_rigor,process_perform,process_review,products_mapped,products_support,products_repo,interative,spdata_dataqual,spdata_abs,spdata_eval,envvar_relevance,envvar_align,process_algo,process_sens,process_rigor,process_perform,process_review,products_mapped,products_support,products_repo,interative,spdata_dataqualNotes,spdata_absNotes,spdata_evalNotes,envvar_relevanceNotes,envvar_alignNotes,process_algoNotes,process_sensNotes,process_rigorNotes,process_performNotes,process_reviewNotes,products_mappedNotes,products_supportNotes,products_repoNotes,interativeNotes"
rubric_default = c("I","A","A","A","A","I","A","A","A","I","A","I","A","A","","","","","","","","","","","","","","")

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
  model_comments = model_comments,
  metaData_comments = metaData_comments,
  modeller = modeller,
  add_vars = add_vars,
  remove_vars = remove_vars,
  rubric_default = rubric_default,
  project_blurb = project_blurb,
  prompt = prompt
)


########## 
##########
##########

# TESTING / DEBUGGING ONLY
library(here)
rm(list=ls())
# Use the lines below for debugging (running line by line) for a certain script
# This loads the variables used in previous model run for the species, 
# so you need to have started a run_SDM() run in step 2 first.

# for scripts 1-3, run just the following 3 lines
model_species <- "anaxexsu"
load(here("_data","species",model_species,"runSDM_paths.Rdata"))
for(i in 1:length(fn_args)) assign(names(fn_args)[i], fn_args[[i]])

# if debugging script 4 or later, also load the specific model output rdata file
model_rdata <- max(list.files(here("_data","species",model_species,"outputs","rdata")))
load(here("_data","species",model_species,"outputs","rdata",paste0(model_rdata)))
