# File: 0_pathsAndSettings.r
# Purpose: to define a set of consistently used objects for a full modeling
#   run. The goal is to avoid redundancy and improve consistency among scripts.


# Set inputs ----
# These locations require data created not as part of these scripts

# The folder that has your species polygon data. 
loc_spPoly <- "E:/SDM/Aquatic/inputs/species/glypmuhl/polygon_data"

# This is the full path and name of the information-tracking database
nm_db_file <- "E:/SDM/Aquatic/databases/SDM_lookupAndTracking.sqlite"

# This is the background random points shapefile, path then name (without the 'shp)
loc_bkgPts <- "E:/SDM/Aquatic/inputs/background"
nm_bkgPts <- "bkgrd_att"

# the folder containing all environmental variable raster (tiffs)
loc_envVars <- "E:/SDM/Aquatic/inputs/env_vars/geotiffs_masked"

# the path where the next two shapefiles are stored
loc_otherSpatial <- "E:/SDM/Aquatic/other_spatial"

# A shapefile showing state boundaries (or other reference boundaries)
# used in the map produced in the metadata
nm_refBoundaries <- "StateBoundariesAlbersConicEqualArea"

# A shapefile showing the extent of the study area over which the model 
# was created. This is also used in the metadata map.
nm_studyAreaExtent <- "reg5_pred_20161027"


# Set destination folders ----
# These locations are initially locations where outputs are written. 
# In many cases, they become input folders later in the process. 

# The folder for species point data
loc_spPts <- "E:/SDM/Aquatic/inputs/species/glypmuhl/point_data"

# output folder for RData files
loc_RDataOut <- "E:/SDM/Aquatic/outputs"

# output folder for grids (raster predictions)
loc_outRas <- "K:/Reg5Modeling_Project/outputs/grids"

# output folder for metadata
loc_outMetadata <- "K:/Reg5Modeling_Project/outputs/metadata"
