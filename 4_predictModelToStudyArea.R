# File: 4_predictModelToStudyArea.r
# Purpose: create the shapefile with model predictions

## start with a fresh workspace with no objects loaded

library(randomForest)
library(data.table)
library(dplyr) 
library(sf)

# load data ----
# get the rdata file
setwd(loc_model)
dir.create(paste0(model_species,"/outputs/model_predictions"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))

# load rdata
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

# load the environmental variables -- analogous to the development of the raster stack in the terr models
presHUC <- stringr::str_pad(as.character(df.full$huc12[df.full$pres==1]), 12, pad = 0)
HUCsubset <- unique(substr(presHUC, 1, huc_level)) # subset to number of huc digits

# SQLite database integration for Env Vars
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT COMID id FROM ",nm_bkg[2]," WHERE ","substr(HUC12,1,",huc_level,") IN ('", paste(HUCsubset, collapse = "','"),"');") # note that the 'substr' is the SQLite version, not R
EnvVars_huc <- dbGetQuery(dbEV, SQLQuery)$id 
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2],"_att WHERE COMID IN ('", paste(EnvVars_huc, collapse = "','"),"')")
EnvVars <- dbGetQuery(dbEV, SQLQuery) 
dbDisconnect(dbEV)

names(EnvVars) <- tolower(names(EnvVars))

# run prediction, using all rows in EnvVars with complete cases----
df.all <- df.full
df.all.pred <- EnvVars[c("comid",names(df.all)[indVarCols])]
df.all.pred <- df.all.pred[complete.cases(df.all.pred),]

result <- as.data.frame(predict(rf.full, df.all.pred[names(df.all)[indVarCols]], type="prob"))

## get probability for presence (column name = "1")
result <- result[,"1"]
results_join_table <- data.frame(comid=df.all.pred$comid, prbblty=result)

# load the reaches from the DB
db <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2]," WHERE COMID IN ('", paste(results_join_table$comid, collapse = "','"),"')") 
shapef1 <- dbGetQuery(db, SQLQuery)
names(shapef1) <- tolower(names(shapef1))

SQLQuery <- paste0("SELECT proj4string p FROM lkpCRS WHERE table_name = '", nm_bkg[2], "';") 
proj4 <- dbGetQuery(db, SQLQuery)$p  # save for HUC12s down below

shapef <- st_sf(shapef1[c("comid", "huc12", "wacomid")], geometry = st_as_sfc(shapef1$wkt), crs = proj4)
try(shapef <- st_sf(shapef1[c("comid", "huc12", "wacomid","strord")], geometry = st_as_sfc(shapef1$wkt), crs = proj4), silent = T)

# join probability to shapefile
shapef <- st_zm(merge(shapef, results_join_table, by = "comid", all.x = F))

# write the shapefile
st_write(shapef, paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results.shp"), delete_layer = T)

###
# load the HUC12s from the database to make a project area boundary
db <- dbConnect(SQLite(),dbname=nm_huc12[1])
SQLQuery <- paste0("SELECT * FROM ",nm_huc12[2], " WHERE ","substr(HUC12,1,",huc_level,") IN ('", paste(HUCsubset, collapse = "','"),"');")
shapef2 <- dbGetQuery(db, SQLQuery)
names(shapef2) <- tolower(names(shapef2))

shapeh <- st_sf(shapef2[c("huc12")], geometry=st_as_sfc(shapef2$wkt), crs=proj4)
#try(shapeh <- st_sf(shapef2[c("huc12")], geometry=st_as_sfc(shapef2$wkt), crs=proj4), silent=T)
#shapeh <- st_buffer(shapeh, 0) # zero-width buffer to deal with random geometry errors
shapehuc <- shapeh # make a copy to generate the huc10s for the review tool.  See below.
shapeh <- st_union(shapeh) # dissolve the polygons
st_write(shapeh, paste0("model_predictions/", modelrun_meta_data$model_run_name, "_modelrange.shp"), delete_layer=T)

# write out a list of HUC10s for the model review tool
shapehuc$huc10 <- substring(shapehuc$huc12,1,10)
listHUC10 <- as.data.frame(unique(shapehuc$huc10))
names(listHUC10)[names(listHUC10) == 'unique(shapehuc$huc10)'] <- 'huc10'
write.csv(listHUC10, paste0("model_predictions/", modelrun_meta_data$model_run_name, "_huc10.csv"), row.names=FALSE)

dbDisconnect(db)



