##
# preproc_populate_HSM_Tracker.R
#
# the purpose of this script is to grab data from the SQLite db and 
# populate the HSM tracker DB

library(here)
library(odbc)
library(RSQLite)

# first get data from the sqlite DB
dbloc <- here("_data","databases")
dbname <- file.path(dbloc, "SDM_lookupAndTracking_AZ.sqlite")
db <- dbConnect(SQLite(), dbname)

sql <- paste("SELECT * ",
             "FROM lkpSpecies;")
dat.in.db <- dbGetQuery(db, statement = sql)
dbDisconnect(db)
rm(db)
# 

# connect up to the tracking db
fn <- here("_data","databases", "hsm_tracker_connection_string_short.dsn")
cn <- dbConnect(odbc::odbc(), .connection_string = readChar(fn, file.info(fn)$size))

# prep for and populate the Elements table
upDat <- dat.in.db[,c("EGT_ID","broad_group","tax_group","scientific_name",
                      "common_name","g_rank","rounded_g_rank","esa_status")]

names(upDat) <- c("ELEMENT_GLOBAL_ID","Broad_Group","Taxonomic_Group","Scientific_Name",
                 "Common_Name","G_Rank","Rounded_G_Rank","ESA_Status")

dbAppendTable(cn, "Elements", upDat, overwrite = FALSE, append = TRUE)


# cutecodes
upDat <- dat.in.db[,c("EGT_ID","sp_code")]
names(upDat) <- c("EGT_ID","cutecode")
dbAppendTable(cn, "Cutecodes", upDat, overwrite = FALSE, append = TRUE)

# model cycle
upDat <- dat.in.db[,c("EGT_ID"), drop = FALSE]
upDat$model_cycle <- rep(1, nrow(upDat))
upDat$init_date <- rep("2021/01/12", nrow(upDat))
dbAppendTable(cn, "ModelCycle", upDat, overwrite = FALSE, append = TRUE)

# Projects
upDat <- dat.in.db[,c("EGT_ID"), drop = FALSE]
upDat$project <- "AZ_SWG"
dbAppendTable(cn, "Projects", upDat, overwrite = FALSE, append = TRUE)


dbDisconnect(cn)
rm(cn)



