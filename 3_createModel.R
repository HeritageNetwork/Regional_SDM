# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(RSQLite)
library(ROCR)    #for ROC plots and stats
library(vcd)     #for kappa stats
library(abind)   #for collapsing the nested lists
library(foreign) #for reading dbf files
library(randomForest)
library(iterators)
library(doParallel)

source(paste0(loc_scripts, "/helper/modelrun_meta_data.R"), local = FALSE) # generates modelrun_meta_data

setwd(loc_model)
dir.create(paste0(model_species,"/outputs/rdata"), recursive = T, showWarnings = F)
setwd(paste0("./",model_species,"/inputs"))

# read data from the att db
dbName <- paste(baseName, "_att.sqlite", sep="")
db <- dbConnect(SQLite(), paste0("model_input/",dbName))

tableName <- paste0(baseName, "_att")
df.in <- dbReadTable(db, tableName)

# get the background data from the DB
tableName <- paste0(nm_bkgPts[2], "_clean")
df.abs <- dbReadTable(db, tableName)
dbDisconnect(db)
rm(db, dbName, tableName)

# set the seed before validation loops
set.seed(seed)

# connect to DB ..
db <- dbConnect(SQLite(),dbname=nm_db_file)
# get species info
SQLquery <- paste("SELECT scientific_name SciName, common_name CommName, sp_code Code, broad_group Type, egt_id, g_rank, rounded_g_rank FROM lkpSpecies WHERE sp_code = '", model_species,"';", sep="")
ElementNames <- as.list(dbGetQuery(db, statement = SQLquery)[1,])

envvar_list <- dbGetQuery(db, "SELECT gridname g from lkpEnvVars;")$g
envvar_list <- tolower(envvar_list)

#also get correlated env var information
SQLquery <- "SELECT gridName, correlatedVarGroupings FROM lkpEnvVars WHERE correlatedVarGroupings IS NOT NULL order by correlatedVarGroupings;"
corrdEVs <- dbGetQuery(db, statement = SQLquery)
dbDisconnect(db)
rm(db, SQLquery)

# are we using the 330 m raster set? if so, rename df.abs
# all 330m raster names begin with "z3"
if(length(grep("z3",names(df.in))) > 0){
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- "SELECT names_30m, names_330m from mapEnvVarDifferentResolutions;"
  envarNames <- dbGetQuery(db, statement = sql, stringsAsFactors = FALSE) 
  envarNames <- data.frame(sapply(envarNames, FUN = function(x) tolower(x)), stringsAsFactors = FALSE)
  names(df.abs) <- tolower(names(df.abs))
  namesDF <- data.frame(absNames = names(df.abs), stringsAsFactors = FALSE)
  namesDF <- merge(namesDF, envarNames, by.x = "absNames", by.y = "names_30m", all.x = TRUE)
  namesDF$names_330m[is.na(namesDF$names_330m)] <- namesDF$absNames[is.na(namesDF$names_330m)]
  # order them, then rename them
  df.abs <- df.abs[,namesDF$absNames]
  names(df.abs) <- namesDF$names_330m
  dbDisconnect(db)
  rm(db, sql, envarNames, namesDF)
  }

#make sure we don't have any NAs
df.in <- df.in[complete.cases(df.in[,!names(df.in) %in% c("obsdate","date")]),]  # to ensure missing dates are not excluding records
df.abs <- df.abs[complete.cases(df.abs),]

# align data sets, QC ----
# add some fields to each
df.in <- cbind(df.in, pres=1)
df.abs$stratum <- "pseu-a"
df.abs <- cbind(df.abs, GROUP_ID="pseu-a", 
					pres=0, RA="high", SPECIES_CD="background")

# lower case column names
names(df.in) <- tolower(names(df.in))
names(df.abs) <- tolower(names(df.abs))

# get an original list of env-vars for later writing to tblVarsUsed
envvar_list <- names(df.abs)[names(df.abs) %in% envvar_list] # gets a list of environmental variables

# get a list of env vars from the folder used to create the raster stack
raslist <- list.files(path = loc_envVars, pattern = ".tif$", recursive = TRUE)

# get short names from the DB
# first shorten names in subfolders (temporal vars).
raslist.short <- unique(unlist(
  lapply(strsplit(raslist, "/"), function(x) {x[length(x)]})
))

db <- dbConnect(SQLite(),dbname=nm_db_file)
SQLQuery <- "select gridName, fileName from lkpEnvVars;"
evs <- dbGetQuery(db, SQLQuery)
# restrict to rasters in folder
shrtNms <- merge(data.frame(fileName = raslist.short), evs)

# get the env vars used by df.in
# assumes all env vars in df.in are accounted for by DB and in ras folders
shrtNms <- shrtNms[tolower(shrtNms$gridName) %in% names(df.in),]

# trust that the desired env vars are in df.in
rasnames <- tolower(shrtNms$gridName)

# get a list of all distance-to env vars
SQLquery <- "SELECT gridName FROM lkpEnvVars WHERE distToGrid = 1;"
dtGrids <- dbGetQuery(db, statement = SQLquery)

# clean up
#options(op)
dbDisconnect(db)
rm(db, SQLQuery, SQLquery, shrtNms)

# Remove irrelevant distance-to grids ----
# check if pres points are VERY far away from any of the dist-to grids
#   (this can cause erroneous, non-biological relationships that should
#    not be driving the model. Group decision to remove.)

# get the ones we are using here
dtRas <- rasnames[rasnames %in% tolower(dtGrids$gridName)]
# what's the closest distance for each?
dtRas.min <- apply(as.data.frame(df.in[,dtRas]), 2, min)
# remove those whose closest distance is greater than 10km
dtRas.sub <- dtRas.min[dtRas.min > 5000]
rasnames <- rasnames[!rasnames %in% names(dtRas.sub)]

rm(dtRas, dtRas.min, dtRas.sub)

# clean up, merge data sets -----
# this is the full list of fields, arranged appropriately
colList <- c("species_cd","group_id","pres","stratum", "ra", rasnames)

# if colList gets modified, 
# also modify the locations for the independent and dependent variables, here
depVarCol <- 3
indVarCols <- c(6:length(colList))

#re-arrange
df.in <- df.in[,colList]
df.abs <- df.abs[,colList]

# row bind the pseudo-absences with the presence points
df.abs$group_id <- factor(df.abs$group_id)
df.full <- rbind(df.in, df.abs)

# reset these factors
df.full$stratum <- factor(df.full$stratum)
df.full$group_id <- factor(df.full$group_id)
df.full$pres <- factor(df.full$pres)
df.full$ra <- factor(tolower(as.character(df.full$ra)))
df.full$species_cd <- factor(df.full$species_cd)

#how many polygons do we have?
numPys <-  nrow(table(df.in$stratum))
#how many EOs do we have?
numEOs <- nrow(table(df.in$group_id))

#initialize the grouping list, and set up grouping variables
#if we have fewer than 5 EOs, move forward with jackknifing by polygon, otherwise
#jackknife by EO.
group <- vector("list")
group$colNm <- ifelse(numEOs < 5,"stratum","group_id")
group$JackknType <- ifelse(numEOs < 5,"polygon","spatial grouping")
if(numEOs < 5) {
  group$vals <- unique(df.in$stratum)
} else {
  group$vals <- unique(df.in$group_id)
}

# make samp size groupings ----
EObyRA <- unique(df.full[,c(group$colNm,"ra")])
EObyRA$sampSize[EObyRA$ra == "very high"] <- 5
EObyRA$sampSize[EObyRA$ra == "high"] <- 4
EObyRA$sampSize[EObyRA$ra == "medium"] <- 3
EObyRA$sampSize[EObyRA$ra == "low"] <- 2
EObyRA$sampSize[EObyRA$ra == "very low"] <- 1
# set the background pts to the sum of the EO samples
# EObyRA$sampSize[EObyRA$group_id == "pseu-a"] <- sum(EObyRA[!EObyRA$group_id == "pseu-a", "sampSize"])

# there appear to be cases where more than one 
# RA is assigned per EO. Handle it here by 
# taking max value
EObySS <- aggregate(EObyRA$sampSize, by=list(EObyRA[,group$colNm]), max)
# set the background pts to the sum of the EO samples
names(EObySS) <- c(group$colNm,"sampSize")
EObySS$sampSize[EObySS[group$colNm] == "pseu-a"] <- sum(EObySS[!EObySS[group$colNm] == "pseu-a", "sampSize"])

sampSizeVec <- EObySS$sampSize
names(sampSizeVec) <- as.character(EObySS[,group$colNm])
rm(EObySS, EObyRA)

# reset sample sizes to number of points, when it is smaller than desired sample size
# This is only relevant when complete.cases may have removed some points from an already-small set of points
totPts <- table(df.full[,group$colNm])
for (i in names(sampSizeVec)) if (sampSizeVec[i] > totPts[i]) sampSizeVec[i] <- totPts[i]
rm(totPts)

#### run the models! ###
for(algo in ensemble_algos){
  print(paste0("building and validating ", algo, " model."))
  scriptToCall <- paste0(algo, "_3_createModel.R")
  source(here("ensemble", scriptToCall))
}

# #another way
# if("rf" %in% ensemble_algos){
#   source(here("ensemble","rf_3_createModel.R"))  
# }


# save the project, return to the original working directory
dir.create(paste0(loc_model, "/", model_species,"/outputs/rdata"), recursive = TRUE, showWarnings = FALSE)
setwd(paste0(loc_model, "/", model_species,"/outputs"))

## testing, clear out as much as possible so S4 objects don't load
## packages automatically
# rm( "add_vars", "addtorow", "algo", "algoLocs", "allVotesPrespts", "alpha.values", 
#     "auc", "auc.summ", "baseName", "colList", "corrdEVs", "curvars", "cutval.rf", 
#     "db", "depVarCol", "df.abs", "df.abs2", "df.full", "df.full.s", "df.full.s.xgb", "df.full.xgb", "df.in", "df.in2", 
#     "dtGrids", "ensemble_algos", "envarPctile", "envvar_list", "EnvVars", 
#     "evRes", "evs", "f.imp", "figSpecs", "fn_args", "folds", "fullSampVec", "g.imp", "grank_desc", 
# "grp", "i", "imp", "impPlot", "increaseAmt", "ind.bool", "indVarCols", 
#     "K.unw", "K.w", "Kappa.unw.summ", "Kappa.w.summ", "kf", "l", "loc_envVars", "loc_model", "loc_scripts", 
#     "ls", "me.cutval", "me.dat", "me.EnvVars", "me.evRes", "me.f.imp", "me.imp", "me.imp.dat", 
#     "me.maxSSS", "me.MTP", "me.out", "me.out.fin", "me.perf.avg", "me.perf.sampled", 
#     "me.summ.table", "me.t.ctoff", "me.t.f", "me.t.importance", "me.t.rocr.pred", "me.trRes", "me.v.kappa", 
#     "me.v.OvAc", "me.v.rocr.auc", "me.v.rocr.pred", "me.v.rocr.pred.restruct", "me.v.rocr.rocplot", "me.v.tss", 
#     "me.v.y", "me.v.y.flat", "me.v.y.flat.sn", "me.v.y.flat.sp", "metaData_comments", "model_comments", "model_comp_name", 
#     "model_rdata", "model_species", "model_start_time", "modeller", "modelrun_meta_data", "MTP", "mtry", 
#     "my_rprofile", "n.var", "new.desc", "nm_bkgPts", "nm_db_file", "nm_HUC_file", "nm_presFile", "nm_refBoundaries", "NSurl", 
#     "ntrees", "numCores", "numEOs", "numPts", "numPys", "op", "ord", "OriginalNumberOfEnvars", "outPth", "OvAc", "OvAc.summ", 
#     "param", "pPlotListLen", "pPlots", "project_blurb", "project_overview", "r_version", "raslist", "raslist.short", 
#     "rasnames", "remove_vars", "repo_head", "rf.find.envars", "rf.perf.avg", "rf.perf.sampled", 
#     "run_SDM", "sampSizeVec", "sampVec", "scaleVec", "scriptToCall", "sdat", "sdm.customComments", 
#     "sdm.customComments.subset", "sdm.dataSources", "sdm.modeler", "sdm.modeluse", "sdm.thresh.info", 
#     "sdm.thresh.merge", "sdm.thresh.table", "sdm.thresholds", "sdm.var.info", "seed", "seed_str", 
#     "sensit", "sensit.summ", "specif", "specif.summ", "specs", "sql", "SQLquery", "ssVec", 
#     "subSampByGp", "subsetNumberofEnvars", "summ.table", "t.ctoff", "t.f", "t.importance", 
#     "t.rocr.pred", "tblModelInputs", "temp", "thermTemp", "treeSubs", "trRes", "trSet.s", 
#     "tss", "tss.summ", "unregister", "used", "v.kappa", "v.me.pred.cut", "v.OvAc", 
#     "v.rf.pred.cut", "v.rocr.auc", "v.rocr.pred", "v.rocr.pred.restruct", "v.rocr.rocplot", 
#     "v.tss", "v.y", "v.y.flat", "v.y.flat.sn", "v.y.flat.sp", "var_names", "varImpDB", 
#     "varNms", "varsImp", "varsSorted", "vStatsxList", "vuStatsList", "x", "xgb.EnvVars", 
#     "xgb.find.envars",  "xgb.impvals", "xgb.perf", "xgb.pPlots", "xgb.pred", 
#     "xgb.summ.table",  "xgbfitControl", "xgbTuneOutput" )


# don't save fn args/vars
for(i in 1:length(modelrun_meta_data)) assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])
ls.save <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("begin_step","rdata","prompt","scrpt",
                                                             "run_steps","prompt","fn_args", names(fn_args))]
save(list = ls.save, file = paste0("rdata/", model_run_name,".Rdata"), envir = environment())

#save(list = ls(), file = "eriogyps_20200611_120037.Rdata")

# write model metadata to db
# tblModelResults
db <- dbConnect(SQLite(),dbname=nm_db_file)  
tblModelResults <- data.frame(model_run_name = model_run_name, 
                              EGT_ID = ElementNames$EGT_ID, 
                              table_code = baseName,
                              internal_comments = model_comments, 
                              metadata_comments = metaData_comments,
                              model_comp_name = model_comp_name, 
                              modeller = modeller,
                              model_start_time = model_start_time, 
                              model_end_time = as.character(Sys.time()),
                              algorithms = paste(ensemble_algos, collapse = ", "),
                              r_version = r_version, repo_head = repo_head, seed = seed)
dbWriteTable(db, "tblModelResults", tblModelResults, append = T)
dbDisconnect(db)

message(paste0("Saved rdata file: '", model_run_name , "'."))

