# File: 0_pathsAndSettings.r
# Purpose: to define a set of consistently used objects for a full modeling
#   run. The goal is to avoid redundancy and improve consistency among scripts.

# NOTE: this has been deprecated by user_run_SDM. The arguments
# below are still used in the scripts, as arguments to `run_SDM`. 

# Set inputs ----
loc_scripts <- "E:/git/dnbucklin/Regional_SDM/"

# These locations require data created not as part of these scripts
loc_model <- "D:/testing_SDM/dev_all/species"

# Modeling database
nm_db_file <- "D:/testing_SDM/dev_all/databases/sdm_tracking_dev_all.sqlite"

# species code (from lkpSpecies in modelling database. This will be the new folder name in loc_model.)
model_species <- "micrmont"

# locations file. Provide the full path; File is copied to modeling folder and timestamped.
nm_presFile <- "D:/SDM/Tobacco/inputs/species/micrmont/polygon_data/micrmont.shp"

# full path to background points
nm_bkgPts = "D:/SDM/Tobacco/inputs/background/tobacco/tobacco_att.shp"

# the folder containing all environmental variable raster (tiffs)
loc_envVars <- "D:/SDM/Tobacco/env_vars/Tobacco"

# A shapefile showing state boundaries (or other reference boundaries)
# used in the map produced in the metadata
nm_refBoundaries = "D:/SDM/Tobacco/other_spatial/shp/StatesEast.shp" # background grey refernce lines in map

# A shapefile showing the extent of the study area over which the model 
# was created. This is also used in the metadata map.
nm_studyAreaExtent = "D:/SDM/Tobacco/other_spatial/shp/sdmVA_pred_20170131.shp" # outline black boundary line for study area in map