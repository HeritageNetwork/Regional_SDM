
# After review by experts a key component of the model review tool
# is that experts point out HUCs out of range for the species. 

# if the model needs to be re-run, these HUCs need to be removed from the
# range table. This script does that. 

library(checkpoint)
checkpoint("2020-04-22", scanForPackages = FALSE)
library(here)
library(RSQLite)
library(odbc)
library(DBI)

## get set up  ----
ccd <- "chiopalaorga"
modelCycle <- 1
nm_db_file <- here("_data","databases","SDM_lookupAndTracking_AZ_phase1spp.sqlite")


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
              "MRTDetailedFeedback.detailed_feedback_comment ",
              "FROM MRTDetailedFeedback ",
              "INNER JOIN v2_ModelCycle ON MRTDetailedFeedback.model_cycle_ID = v2_ModelCycle.ID")

allHucActions <- dbGetQuery(cn, sql)

dbDisconnect(cn)
rm(cn)

# get the hucs we want removed
# get cutecode by stripping model tail because cutecodes may have hyphens or underscores in them now
allHucActions$ccode <- sub("_[0-9]{8}_[0-9]{6}","", hucActions$model_version)

oneSppHucActions <- allHucActions[allHucActions$ccode == ccd & allHucActions$model_cycle == 1,]

oneSppHucRemoves <- oneSppHucActions[oneSppHucActions$huc_status_type == 2,]

# connect to the sqlite db ----

db <- dbConnect(SQLite(),dbname=nm_db_file)

sql <- paste0("SELECT sp_code, EGT_ID, location_use_class ",
              "FROM lkpSpecies WHERE ",
              "sp_code ='", ccd, "';")

sppDat <- dbGetQuery(db, statement = sql)

if(nrow(sppDat) > 1){
  stop("too many rows, bad selection")
}


# try a select

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

dbDisconnect(db)
rm(db)


# TODO: apply HUC additions


