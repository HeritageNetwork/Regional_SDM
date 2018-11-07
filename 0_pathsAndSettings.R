# File: 0_pathsAndSettings.r
# Purpose: to define a set of consistently used objects for a full modeling
#   run. The goal is to avoid redundancy and improve consistency among scripts.


# Set inputs ----
# These locations require data created not as part of these scripts

# The folder that has your species csv data
loc_spReaches <- "E:/SDM/Aquatic2/inputs/species/alasvari/reach_data" # replaces the polygon input

# This is the full path and name of the information-tracking database
nm_db_file <- "E:/SDM/Aquatic2/databases/SDM_lookupAndTracking_new.sqlite"

# This is the background random points shapefile, path then name (without the 'shp)
loc_bkgReach <- "E:/SDM/Aquatic2/inputs/species/alasvari/background"
# nm_bkgPts <- "bkgrd_att"

# the folder containing all environmental variable table (csv)
loc_envVars <- "E:/SDM/Aquatic2/env_vars"

# the path where the next two shapefiles are stored
loc_otherSpatial <- "E:/SDM/Aquatic2/other_spatial"

# A shapefile containing all flowlines in the study area (used for background)
nm_allflowlines <- "PA_all_flowlines"

# A shapefile showing state boundaries (or other reference boundaries)
# used in the map produced in the metadata
nm_refBoundaries <- "states"

# A shapefile showing the extent of the study area over which the model 
# was created. This is also used in the metadata map.
nm_studyAreaExtent <- "PA_watersheds"

# Set destination folders ----
# These locations are initially locations where outputs are written. 
# In many cases, they become input folders later in the process. 

# The folder for species point data
# loc_spPts <- "E:/SDM/Aquatic/inputs/species/glypmuhl/point_data"

# output folder for RData files
loc_RDataOut <- "E:/SDM/Aquatic2/outputs/rdata"

# output folder for grids (raster predictions)
loc_outVector <- "E:/SDM/Aquatic2/outputs/shapefiles"

# output folder for metadata
loc_outMetadata <- "E:/SDM/Aquatic2/outputs/metadata"

