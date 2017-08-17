# File: 0_pathsAndSettings.r
# Purpose: to define a set of consistently used objects for a full modeling
#   run. The goal is to avoid redundancy and improve consistency among scripts.


# Set inputs ----
# These locations require data created not as part of these scripts

# The folder that has your species csv data
loc_spReaches <- "D:/SDM/Tobacco/inputs/species/chrotenn/reach_data" # replaces the polygon input

# This is the full path and name of the information-tracking database
nm_db_file <- "D:/SDM/Tobacco/databases/VA_Spp/SDM_VA_Tracking_Modeling.sqlite"

# This is the background random points shapefile, path then name (without the 'shp)
loc_bkgReach <- "D:/SDM/Tobacco/inputs/species/chrotenn/background"
# nm_bkgPts <- "bkgrd_att"

# the folder containing all environmental variable table (csv)
loc_envVars <- "D:/SDM/Tobacco/env_vars/Tobacco_aqua"

# the path where the next two shapefiles are stored
loc_otherSpatial <- "D:/SDM/Tobacco/other_spatial/shp/aqua"

# A shapefile containing all flowlines in the study area (used for background)
nm_allflowlines <- "all_VA_flowlines_wHUC12"

# A shapefile showing state boundaries (or other reference boundaries)
# used in the map produced in the metadata
nm_refBoundaries <- "StatesEast"

# A shapefile showing the extent of the study area over which the model 
# was created. This is also used in the metadata map.
nm_studyAreaExtent <- "sdmVA_pred_20170131"

# Set destination folders ----
# These locations are initially locations where outputs are written. 
# In many cases, they become input folders later in the process. 

# The folder for species point data
# loc_spPts <- "E:/SDM/Aquatic/inputs/species/glypmuhl/point_data"

# output folder for RData files
loc_RDataOut <- "D:/SDM/Tobacco/outputs/chrotenn/rdata"

# output folder for grids (raster predictions)
loc_outVector <- "D:/SDM/Tobacco/outputs/chrotenn/shapefiles"

# output folder for metadata
loc_outMetadata <- "D:/SDM/Tobacco/outputs/chrotenn/metadata"
