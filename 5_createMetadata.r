# File: 5_createMetadata.r
# Purpose: to summarize validation data and other information about the 
# model and write it to a pdf. This pdf should accompany ALL sharing/showing
# of the SDM map.

# For knitr to work, you need MikTex installed. See http://miktex.org/

# load libraries ----
library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(randomForest)
library(knitr)
library(dplyr)
library(sf)
library(RColorBrewer)
library(classInt)
library(RSQLite)
library(xtable)
library(tinytex)
library(stringi)
library(tables)
library(tmap)
library(tmaptools)
library(OpenStreetMap)

### find and load model data ----
## three lines need your attention. The one directly below (loc_scripts),
## about line 35 where you choose which Rdata file to use,
## and about line 46 where you choose which record to use

setwd(loc_model)
dir.create(paste0(model_species,"/outputs/metadata"), recursive = T, showWarnings = F)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/", modelrun_meta_data$model_run_name,".Rdata"))

# get reach data for the map
results_shape <- st_read(paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results.shp"), quiet = T) # shapefile results for mapping

# get background poly data for the map (study area, reference boundaries, and aquatic areas)
studyAreaExtent <- st_read(here("_data","species",model_species,"outputs","model_predictions",paste0(model_run_name, "_modelrange.shp")), quiet = T)
referenceBoundaries <- st_read(nm_refBoundaries, quiet = T)


if (!is.null(nm_aquaArea)) {
  wacomid <- as.numeric(unique(as.character(results_shape$wacomid)))
  wacomid <- wacomid[!is.na(wacomid)]
  if (length(wacomid) > 0) { # on create the waterbodies if some are present
    db <- dbConnect(SQLite(),dbname=nm_aquaArea[1])
    SQLQuery <- paste0("SELECT * FROM ",nm_aquaArea[2]," WHERE COMID IN ('", paste(wacomid, collapse = "','"),"')") 
    shapef2 <- dbGetQuery(db, SQLQuery) # load the waterbodies from the DB
    names(shapef2) <- tolower(names(shapef2))
    SQLQuery <- paste0("SELECT proj4string p FROM lkpCRS WHERE table_name = '", nm_aquaArea[2], "';") 
    proj4 <- dbGetQuery(db, SQLQuery)$p  
    aquaPolys <- st_sf(shapef2[c("comid")], geometry=st_as_sfc(shapef2$wkt), crs = proj4)
    #try(shapef <- st_sf(shapef1[c("comid", "huc12", "wacomid","strord")], geometry = st_as_sfc(shapef1$wkt), crs = proj4), silent = T)
    st_write(aquaPolys, paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results_aquaPolys.shp"), delete_layer = T)
  } else {
    nm_aquaArea <- NULL
  }
}

## Get Program and Data Sources info ----
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)  
SQLquery <- paste("Select lkpModelers.ProgramName, lkpModelers.FullOrganizationName, ",
                  "lkpModelers.City, lkpModelers.State, lkpSpecies.sp_code ",
                  "FROM lkpModelers ", 
                  "INNER JOIN lkpSpecies ON lkpModelers.ModelerID=lkpSpecies.ModelerID ", 
                  "WHERE lkpSpecies.sp_code='", model_species, "'; ", sep="")
sdm.modeler <- dbGetQuery(db, statement = SQLquery)
# NOTE: use column should be populated with 1/0 for sources of data used
SQLquery <- paste("SELECT sp.sp_code, sr.ProgramName, sr.State ",
                  "FROM lkpSpecies as sp ",
                  "INNER JOIN mapDataSourcesToSpp as mp ON mp.EGT_ID=sp.EGT_ID ",
                  "INNER JOIN lkpDataSources as sr ON mp.DataSourcesID=sr.DataSourcesID ",
                  # "WHERE mp.use = 1 ",
                  "AND sp.sp_code ='", model_species, "'; ", sep="")
sdm.dataSources <- dbGetQuery(db, statement = SQLquery)
sdm.dataSources <- sdm.dataSources[order(sdm.dataSources$ProgramName),]

SQLquery <- paste("SELECT model_end_time date, egt_id, metadata_comments comments",
                  " FROM tblModelResults ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
sdm.customComments <- dbGetQuery(db, statement = SQLquery)
# assume you want the most recently entered comments, if there are multiple entries
if(nrow(sdm.customComments) > 1) {
  sdm.customComments <- sdm.customComments[order(sdm.customComments$date, decreasing = TRUE),]
  sdm.customComments.subset <- sdm.customComments[1,]
} else {
  sdm.customComments.subset <- sdm.customComments
}

## Get threshold information ----
SQLquery <- paste("Select ElemCode, dateTime, cutCode, cutValue, capturedEOs, capturedPolys, capturedPts ", 
                  "FROM tblModelResultsCutoffs ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
sdm.thresholds <- dbGetQuery(db, statement = SQLquery)

# get info about thresholds
SQLquery <- paste("SELECT cutCode, cutFullName, cutDescription, cutCitationShort, cutCitationFull, sortOrder ", 
                  "FROM lkpThresholdTypes ", 
                  "WHERE cutCode IN (", 
                  toString(sQuote(sdm.thresholds$cutCode)),
                  ");", sep = "")
sdm.thresh.info <- dbGetQuery(db, statement = SQLquery)

sdm.thresh.merge <- merge(sdm.thresholds, sdm.thresh.info)
#sort it
sdm.thresh.merge <- sdm.thresh.merge[order(sdm.thresh.merge$sortOrder),]
sdm.thresh.table <- sdm.thresh.merge[,c("cutFullName", "cutValue", "capturedEOs", "capturedPts", "cutDescription")]
names(sdm.thresh.table) <- c("Threshold", "Value", "Groups", "Pct","Description")
sdm.thresh.table$Description <- gsub("points", "reaches", sdm.thresh.table$Description, fixed=TRUE) # hack to change points -> reaches
sdm.thresh.table$Groups <- paste(round(sdm.thresh.table$Groups/numEOs*100, 1), "(",sdm.thresh.table$Groups, ")", sep="")
#sdm.thresh.table$Polys <- paste(round(sdm.thresh.table$Polys/numPys*100, 1),  "(",sdm.thresh.table$Polys, ")", sep="")
numPts <- nrow(subset(df.full, pres == 1))
sdm.thresh.table$Pct <- paste(round(sdm.thresh.table$Pct/numPts*100, 1),  sep="")

## get grank definition ----
SQLquery <- paste0("SELECT rank, rankname FROM lkpRankDefinitions where rank = '",ElementNames$rounded_g_rank,"';", sep="")
grank_desc <- dbGetQuery(db, SQLquery)

# make a url to NatureServe Explorer
NSurl <- paste("http://explorer.natureserve.org/servlet/NatureServe?searchName=",gsub(" ", "+", ElementNames[[1]], fixed=TRUE), sep="")

## get Model Evaluation and Use data ----
SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, interative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, interativeNotes ", 
                  "FROM lkpSpeciesRubric ", 
                  "WHERE sp_code ='", model_species, "'; ", sep="")
sdm.modeluse <- dbGetQuery(db, statement = SQLquery)
sdm.modeluse[is.na(sdm.modeluse)] <- " "
sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpet with Caution"

## Get env. var lookup table ----
SQLquery <- paste0("SELECT gridName g from tblModelResultsVarsUsed where model_run_name = '",
                   model_run_name, "' and inFinalModel = 1;")
var_names <- dbGetQuery(db, SQLquery)$g
SQLquery <- paste("SELECT fullName, description ",
                  "FROM lkpEnvVarsAqua ",
                  "WHERE gridName COLLATE NOCASE IN (",
                  toString(sQuote(var_names)),
                  ") ORDER BY fullName;", sep = "")
sdm.var.info <- dbGetQuery(db, statement = SQLquery)
names(sdm.var.info) <- c("Variable Name","Variable Description")

# escape symbols for latex
ls <- c("&","%","$","#","_","{","}")
for (l in ls) {
  sdm.var.info$`Variable Name` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Name`, fixed = T)
  sdm.var.info$`Variable Description` <- gsub(l, paste0("\\",l), sdm.var.info$`Variable Description`, fixed = T)
}
# replace degree symbols
for (l in 1:length(sdm.var.info$`Variable Description`)) {
  new.desc <- stri_escape_unicode(sdm.var.info$`Variable Description`[l])
  if (grepl("\\u00b0",new.desc, fixed = T)) 
    sdm.var.info$`Variable Description`[l] <- gsub("\\u00b0", "$^\\circ$", new.desc, fixed = T)
}
# put descriptions in parboxes for multiple lines
sdm.var.info$`Variable Description` <- paste0("\\parbox{20cm}{",sdm.var.info$`Variable Description`,"}")

# fix greater than and less than symbol in rubric table
sdm.modeluse$process_performNotes <- gsub(">=","$\\\\geq$", sdm.modeluse$process_performNotes)
sdm.modeluse$process_performNotes <- gsub("<","$<$ ", sdm.modeluse$process_performNotes)


## Run knitr and create metadata ----

# Also, might need to run this twice. First time through tex builds the reference
# list, second time through it can then number the refs in the doc.

setwd("metadata")
#knit2pdf(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))  # using this causes a UTF8 error
knit(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
call <- paste0("pdflatex -interaction=nonstopmode ",model_run_name , ".tex")
system(call)
system(call) # 2nd run to apply citation numbers

# delete .txt, .log etc if pdf is created successfully.
fn_ext <- c(".log",".aux",".out") 
if (file.exists(paste(model_run_name, ".pdf",sep=""))){
  #setInternet2(TRUE)
  #download.file(fileURL ,destfile,method="auto")
  for(i in 1:NROW(fn_ext)){
    fn <- paste(model_run_name, fn_ext[i],sep="")
    if (file.exists(fn)){ 
      file.remove(fn)
    }
  }
}

## clean up ----
dbDisconnect(db)
options(op)

