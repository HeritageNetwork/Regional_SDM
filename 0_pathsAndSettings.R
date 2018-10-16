# File: 0_pathsAndSettings.r
# Purpose: to define a set of consistently used objects for a full modeling
#   run. The goal is to avoid redundancy and improve consistency among scripts.

# assumptions:
#  1. your repository has a _data folder and the primary database is in _data/databases
#  2. original species data are in _data/inputs. Derived/attributed sets will be moved to run-specific folders
#  3. background data are in _data/inputs/background
#  4. other spatial data (study area extent, state boundaries) are in _data/other_spatial

library(here)

# Set inputs ----
# These locations require data created not as part of these scripts
#### still need these ##

model_species <- "bombus_test"

nm_presPts <- "ma_test_points.shp"
nm_presPolys <- "ma_test_polys.shp"

# This is the background random points shapefile, path then name (without the 'shp)
nm_bkgPts <- "ma_test_background.shp"

# the folder containing all environmental variable raster (tiffs)
loc_envVars <- "K:/Reg5Modeling_Project/inputs/env_vars/geotiffs_masked"

# A shapefile showing state boundaries (or other reference boundaries)
# used in the map produced in the metadata
nm_refBoundaries <- "StateBoundariesAlbersConicEqualArea"

# A shapefile showing the extent of the study area over which the model 
# was created. This is also used in the metadata map.
nm_studyAreaExtent <- "reg5_pred_20161027"




# 
# #### these should no longer be needed here ##
# # The folder that has your species polygon data. 
# loc_spPoly <- here("_data/inputs")
# 
# # This is the full path and name of the information-tracking database
# nm_db_file <- here("_data/databases/SDM_lookupAndTracking.sqlite")
# 
# # This is the background random points shapefile, path then name (without the 'shp)
# loc_bkgPts <- here("_data/inputs/background")
# 
# # the path where the next two shapefiles are stored
# loc_otherSpatial <- here("_data/other_spatial")
# 
# # Set destination folders ----
# # These locations are initially locations where outputs are written. 
# # In many cases, they become input folders later in the process. 
# 
# # The folder for species point data
# loc_spPts <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/point_data"
# 
# # output folder for RData files
# loc_RDataOut <- "K:/Reg5Modeling_Project/outputs"
# 
# # output folder for grids (raster predictions)
# loc_outRas <- "K:/Reg5Modeling_Project/outputs/grids"
# 
# # output folder for metadata
# loc_outMetadata <- "K:/Reg5Modeling_Project/outputs/metadata"
