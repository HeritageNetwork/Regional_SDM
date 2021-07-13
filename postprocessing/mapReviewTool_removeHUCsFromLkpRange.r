
# After review by experts a key component of the model review tool
# is that experts point out HUCs out of range for the species. 

# if the model needs to be re-run, these HUCs need to be removed from the
# range table. This script does that. 

library(here)
library(RSQLite)
library(odbc)
library(DBI)

library(sf)

## get set up  ----
ccd <- "leoppard"
modelCycle <- 1
nm_db_file <- here("_data","databases","SDM_lookupAndTracking_AZ_phase1spp.sqlite")

db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT sp_code, EGT_ID, location_use_class ",
              "FROM lkpSpecies WHERE ",
              "sp_code ='", ccd, "';")
sppDat <- dbGetQuery(db, statement = sql)

if(nrow(sppDat) > 1){
  stop("too many rows, bad selection")
}


## get MRT data from HSM Tracking database ----
fn <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(fn, file.info(fn)$size))
sql <- paste0("SELECT v2_ModelCycle.ID, v2_ModelCycle.model_cycle, ",
              "MRTDetailedFeedback.model_cycle_ID, ",
              "MRTDetailedFeedback.model_version, ",
              "MRTDetailedFeedback.speciesMasterLookupKey, ",
              "MRTDetailedFeedback.mrt_cutecode, ",
              "MRTDetailedFeedback.mrt_UserID, ",
              "MRTDetailedFeedback.huc_status_type, ",
              "MRTDetailedFeedback.huc_id, ",
              "MRTDetailedFeedback.edit_date, ",
              "MRTDetailedFeedback.detailed_feedback_comment ",
              "FROM MRTDetailedFeedback ",
              "INNER JOIN v2_ModelCycle ON MRTDetailedFeedback.model_cycle_ID = v2_ModelCycle.ID")
allHucActions <- dbGetQuery(cn, sql)

dbDisconnect(cn)
rm(cn)

# get the hucs we want action on ----
# get cutecode by stripping model tail because cutecodes may have hyphens or underscores in them now
allHucActions$ccode <- sub("_[0-9]{8}_[0-9]{6}","", allHucActions$model_version)

oneSppHucActions <- allHucActions[allHucActions$ccode == ccd & allHucActions$model_cycle == modelCycle,]

oneSppHucRemoves <- oneSppHucActions[oneSppHucActions$huc_status_type == 2,]
oneSppHucAdds <- oneSppHucActions[oneSppHucActions$huc_status_type == 1,]

# if you want to write out a shapefile of these 'removes' apply the next lines ----
fp <- file.path("G:","_Projects","AZGFD","Regional_SDM","_data","other_spatial","feature")
allHucs <- st_read(file.path(fp, "HUC10.shp"))
hucRems <- allHucs[allHucs$HUC10 %in% oneSppHucRemoves$huc_id,]
fp <- file.path("G:","_Projects","AZGFD","Users","tim","HUC_checks")
st_write(hucRems, file.path(fp, paste0(ccd,"_expert_removes_hucs.shp")), append = FALSE)

##
# apply the remove actions ----
##

# do a select to get range_ids
sql <- paste0("SELECT range_id, EGT_ID, huc10_id, location_use_class ",
              "FROM lkpRange WHERE EGT_ID = ", sppDat$EGT_ID[[1]], 
              " AND location_use_class = '", sppDat$location_use_class, "' ",
              " AND huc10_id IN (", 
              toString(shQuote(oneSppHucRemoves$huc_id)),
              ");")
# convert any NA to is null
sql <- gsub("= 'NA'","IS NULL",sql)
hucDat <- dbGetQuery(db, statement = sql)         

# now delete based on range_id ----
sql <- paste0("DELETE FROM lkpRange WHERE range_id IN (",
              toString(shQuote(hucDat$range_id)), ");"
              )
dbExecute(db, statement = sql)

###
# apply the add actions ----
###
# first, have they already been added?
# do a select to get range_ids
sql <- paste0("SELECT range_id, EGT_ID, huc10_id, location_use_class ",
              "FROM lkpRange WHERE EGT_ID = ", sppDat$EGT_ID[[1]], 
              " AND location_use_class = '", sppDat$location_use_class, "' ",
              " AND huc10_id IN (", 
              toString(shQuote(oneSppHucAdds$huc_id)),
              ");")
# convert any NA to is null
sql <- gsub("= 'NA'","IS NULL",sql)
hucDat <- dbGetQuery(db, statement = sql)
# remove the existing ones, if any
if(nrow(hucDat) > 0) {
  oneSppHucAdds <- oneSppHucAdds[!oneSppHucAdds$huc_id %in% hucDat$huc10_id,]
 }
# remove duplicates in list

oneSppHucAdds <- oneSppHucAdds[!duplicated(oneSppHucAdds$huc_id),]

toAdd <- oneSppHucAdds[,c("huc_id","detailed_feedback_comment")]
names(toAdd) <- c("huc10_id", "comments")
toAdd$EGT_ID <- sppDat$EGT_ID[[1]]
toAdd$location_use_class <- sppDat$location_use_class[[1]]
toAdd$version_info <- paste("MRT feedback from ", format(oneSppHucAdds$edit_date, format = "%Y-%m-%d"))
toAdd$origin <- "MRT feedback"

toAdd <- toAdd[,c("EGT_ID","huc10_id","origin","location_use_class", "version_info","comments")]

dbAppendTable(db, "lkpRange", toAdd)

##
# get remaining range (to check in arc) ----
##
sql <- paste0("SELECT range_id, EGT_ID, huc10_id, location_use_class ",
              "FROM lkpRange WHERE EGT_ID = ", sppDat$EGT_ID[[1]], 
              " AND location_use_class = '", sppDat$location_use_class, "'; ")
# convert any NA to is null
sql <- gsub("= 'NA'","IS NULL",sql)

hucDat <- dbGetQuery(db, statement = sql)  

fp <- file.path("G:","_Projects","AZGFD","Regional_SDM","_data","other_spatial","feature")
# slow, probably already did this above
#allHucs <- st_read(file.path(fp, "HUC10.shp"))

hucSubS <- allHucs[allHucs$HUC10 %in% hucDat$huc10_id,]

nrow(hucSubS)
nrow(hucDat)

fp <- file.path("G:","_Projects","AZGFD","Users","tim","HUC_checks")
st_write(hucSubS, file.path(fp, paste0(ccd,"_hucs.shp")), delete_layer = TRUE)



#clean up ----
dbDisconnect(db)
rm(db)



