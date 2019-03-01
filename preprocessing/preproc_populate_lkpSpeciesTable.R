##
# preproc_populate_lkpSpeciesTable.R
#
# the purpose of this script is to grab data from another source and populate
# the species lookup table in the sqlite DB.

library(here)
library(RODBC)
library(RSQLite)

# in this project, the master species list is here
# N:/tracking_database
# and named
# MoBI_Tracking_currentVersion.accdb

# set up an ODBC connection for RODBC to use. You only need to do this once, 
# if you continue to connect to the db with the same name

# Windows: click magnifier (search) in lower left, type "ODBC" in search window, open
# "ODBC Data Sources (64 bit)". On User DSN tab, choose "Add", then choose
# "Microsoft Access Driver (*.mdb,*.accdb)", Finish 
# In Data Source Name, put "mobi_spp_tracking", then select
# the DB using the Select button. "OK", then close out. 

cn <- odbcConnect("mobi_spp_tracking")

# get records from the FinalSppLists table 
sql <- "Select ELEMENT_GLOBAL_ID, \"Broad Group\", \"Taxonomic Group\",
        \"Scientific Name\", \"Common Name\", G_RANK, \"Rounded G-Rank\",
        \"ESA Status\"
        from FinalSppList;"
sppList <- sqlQuery(cn, sql)
head(sppList)
names(sppList) <- c("EGT_ID","broad_group","tax_group","scientific_name","common_name","g_rank",
                    "rounded_g_rank","esa_status")

# get records from the SpeciesWorkFlow table 
sql <- "Select EGT_ID, include_in_mobi, model_type, cutecode from SpeciesWorkFlow;"
sppWorkFlow <- sqlQuery(cn, sql)
names(sppWorkFlow) <- c("EGT_ID","include_in_mobi","model_type","sp_code")
sppWorkFlow$model_type <- toupper(sppWorkFlow$model_type)

# add modtype based on a lookup table for model_type
# add modelerID using vals from lkpModelers. 1=VA, 3=PA, 4=NY
lkp <- data.frame(model_type = c("TER","AQU","BOTH","SUB"), 
                  modtype = c("T","A","B","S"),
                  ModelerID = c(4,3,4,1))
### this is the table that should get used for aquatic modeling !!
## modelerID needs to be PA for "B"
## uncomment if creating lkpSpecies for Aquatics
# lkp <- data.frame(model_type = c("TER","AQU","BOTH","SUB"), 
#                   modtype = c("T","A","B","S"),
#                   ModelerID = c(4,3,3,1))

sppWF <- merge(sppWorkFlow, lkp)

# get it all together
allDat <- merge(sppList, sppWF)
nrow(allDat)

# remove the do-not-model species
allDat <- allDat[!allDat$include_in_mobi == 2, ]
nrow(allDat)

# add the comments column
allDat$Comment <- ""
head(allDat)

# get the column order right
colOrder <- c("EGT_ID","sp_code","broad_group","tax_group","scientific_name","common_name",
              "g_rank","rounded_g_rank","esa_status","ModelerID","Comment","modtype")

allDat <- allDat[,colOrder]

# push it into the db
# BACK UP your current version!!
# THIS OVERWRITES the current table
dbloc <- here("_data","databases")
dbname <- "SDM_lookupAndTracking.sqlite"
db <- dbConnect(SQLite(), paste0(dbloc, "/", dbname))

dbWriteTable(db, "lkpSpecies", allDat, overwrite = TRUE)

close(cn)
dbDisconnect(db)
rm(db, cn)



