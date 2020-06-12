# File: 5_createMetadata.r
# Purpose: to summarize validation data and other information about the 
# model and write it to a pdf. This pdf should accompany ALL sharing/showing
# of the SDM map.

# For knitr to work, you need MikTex installed. See http://miktex.org/

# load libraries ----
library(ROCR)  #July 2010: order matters, see http://finzi.psych.upenn.edu/Rhelp10/2009-February/189936.html
library(xtable)
library(randomForest)
library(knitr)
library(raster)
library(maptools)
library(dplyr)
library(sf)
library(RColorBrewer)
library(rasterVis)
library(RSQLite)
library(stringi)
library(tables)

library(tmap)
library(tmaptools)
library(OpenStreetMap)

library(tidyr)
library(ggplot2)

### find and load model data ----
#setwd(loc_model)
dir.create(file.path(loc_model, model_species, "outputs","metadata"), recursive = TRUE, showWarnings = FALSE)
setwd(file.path(loc_model, model_species, "outputs"))

model_run_name <- modelrun_meta_data$model_run_name

load(file.path(loc_model, model_species, "outputs","rdata", paste0(modelrun_meta_data$model_run_name,".Rdata")))


##
## create table 1, summary of input data ----
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelInputs where model_run_name = '", 
              model_run_name, "';")
inputs <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

# Assume groups and pres inputs are the same among algorithms
# but background inputs vary
summ.table <- data.frame(
  Sample=c("Presence locations (groups)",
        "Subsamples within groups",
        "Total presence inputs",
        paste0("Background inputs - ", inputs$algorithm)),
  Count=c(
    ifelse(inputs$jckn_grp_column[[1]] == "stratum", 
           inputs$feat_count[[1]],
           inputs$feat_grp_count[[1]]
           ),
    inputs$mn_grp_subsamp[[1]],
    inputs$tot_obs_subsamp[[1]],
    paste0(inputs$tot_bkgd_subsamp)
     ))
# summ.table is what gets used in knitr file
rm(db, sql)

##
## create table 2, summary of validation statistics ----
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsValidationStats where model_run_name = '", 
              model_run_name, "';")
vstats <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

metricsToGet <- c("AUC","Sensitivity","Specificity","TSS")
colsToGet <- c("algorithm","metric","metric_mn","metric_sd")
vstats <- vstats[vstats$metric %in% metricsToGet,colsToGet]
names(vstats) <- c("algorithm","metric","mean","SD")

vstatsList <- split(vstats, f = vstats$algorithm)
vstatsList <- lapply(vstatsList, FUN = function(x) x[,!names(x)=="algorithm"])
attr(vstatsList, "subheadings") <- paste0("Algorithm = ", names(vstatsList))
vStatsxList <- xtableList(vstatsList)
# vStatsxList is what gets used in knitr file
rm(db, sql, metricsToGet, colsToGet, vstatsList)

##
## create data for thermometer figure ----
summaryTSS <- mean(vstats[vstats$metric == "TSS", "mean"])

# 
# print(xtableList(vStatsxList, type="html", file="table2.html"))
# x <- print(vStatsxList, file="table2.tex", include.rownames=FALSE)
# 
# dput(vStatsxList)
# 
# getwd()
# vStatsxList <- dget(file = "testDput.r")
# library(xtable)


##
## create table with all other modeling input settings ----
#  this is placed at the end in Appendix 2
#first get envars counts
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
              model_run_name, "';")
varsUsedStats <- dbGetQuery(db, statement = sql)
dbDisconnect(db)
varsUsedStats <- varsUsedStats[varsUsedStats$inFinalModel == 1,]
vuStats <- aggregate(varsUsedStats$inFinalModel, by = list(varsUsedStats$algorithm), sum)
names(vuStats) <- c("algo","value")

vuStats <- cbind("Name"= "number of predictors used", vuStats)
vuStatsList <- split(vuStats, f = vuStats$algo)
vuStatsList <- lapply(vuStatsList, FUN = function(x) x[,!names(x)=="algo"])

for(algo in names(vuStatsList)){
  if(algo == "me"){
    medat <- data.frame(Name = c("linear feature type used",
                                 "product feature type used",
                                 "quadratic feature type used",
                                 "hinge feature type used"),
               value = c("yes","yes","yes","yes"))
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], medat)
  }
  if(algo == "xgb"){
    xgbdat <- data.frame(Name = c(
                                  "iterations",
                                  "eta",
                                  "max depth",
                                  "gamma",
                                  "colsample by tree",
                                  "min child weight",
                                  "subsample",
                                  "objective"),
                         value = c(
                                   xgb.full$niter,
                                   xgb.full$params$eta,
                                   xgb.full$params$max_depth,
                                   xgb.full$params$gamma,
                                   xgb.full$params$colsample_bytree,
                                   xgb.full$params$min_child_weight,
                                   xgb.full$params$subsample,
                                   xgb.full$params$objective
                                   ))
    #paste0(deparse(xgb.full$call), collapse = ""),
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], xgbdat)
  }
  if(algo == "rf"){
    rfdat <- data.frame(Name = c(
                                "mtry",
                                "number of trees",
                                "type of trees"
                              ),
                        value = c(
                                rf.full$mtry,
                                rf.full$ntree,
                                rf.full$type
                              )
    )
    vuStatsList[[algo]] <- rbind(vuStatsList[[algo]], rfdat)
    }
}
# vuStatsList is what gets used in knitr file
rm(db, sql, varsUsedStats, vuStats, medat, xgbdat, rfdat, algo)

##
## build ROC plot ----
#### this is all in the rnw, possibly change to ggplot, then build it here and print it there
# build lookup for line details
figSpecs <- data.frame(algos = c("rf","me","xgb"),
                       col = c("blue","red","black"),
                       lwd = c(3,2,1),
                       lty = c(1,1,1),
                       stringsAsFactors = FALSE)
# 
# # first a blank plot
# plot(0.5,0.5,lwd=0, col = "white", xlim = c(0,1), ylim = c(0,1),
#      xlab="Avg. false positive rate", ylab="Avg. true positive rate")
# # now add each that was run  
# #if no validation, then this will fail. Used to use if(exists("rf.perf"))...
# for(algo in ensemble_algos){
#   if(algo == "rf" & exists("rf.perf")){
#     specs <- figSpecs[match(algo,figSpecs$algos),]
#     plot(rf.perf,
#          lwd=specs$lwd, 
#          col=specs$col,
#          avg="threshold", 
#          add = TRUE)
#   }
#   if(algo == "me" & exists("me.perf")){
#     specs <- figSpecs[match(algo,figSpecs$algos),]
#     plot(me.perf, 
#          lwd=specs$lwd, 
#          col=specs$col,
#          avg="threshold", 
#          add = TRUE)
#   }
#   if(algo == "xgb"){
#     xgb.pred <- prediction(xgbFit1$pred$pres, xgbFit1$pred$obs)
#     xgb.perf <- performance(xgb.pred, "tpr","fpr")
#     specs <- figSpecs[match(algo,figSpecs$algos),]
#     plot(xgb.perf, 
#          lwd=specs$lwd, 
#          col=specs$col,
#          avg="threshold", 
#          add = TRUE)
#   }
# }
# #possible else if no models were validated: text(0.5,0.5, "No evaluation")
# 
# legend("bottomright", ensemble_algos, 
#        col = figSpecs[match(ensemble_algos,figSpecs$algos),"col"],
#        lwd = figSpecs[match(ensemble_algos,figSpecs$algos),"lwd"],
#        lty = figSpecs[match(ensemble_algos,figSpecs$algos),"lty"],
#        text.col = "black", 
#        merge = TRUE, bg = "gray95")
#   
# 


##
## build importance plot ----

# get the vars used for each
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * from tblModelResultsVarsUsed where model_run_name = '", 
              model_run_name, "';")
varsImp <- dbGetQuery(db, statement = sql)
# get full names
sql <- "SELECT gridName, fullName FROM lkpEnvVars"
varNms <- dbGetQuery(db, statement = sql)
dbDisconnect(db)

# remove vars not used by any algo
varsImp <- varsImp[varsImp$inFinalModel == 1,]
#varsImpUnqV <- unique(varsImp$gridName)

# merge in full name, reduce cols
varsImp <- merge(varsImp, varNms)
varsImp <- varsImp[,c("algorithm","fullName","impVal")]

# # convert to wide format 
# varsImp.s <- spread(varsImp, algorithm, impVal)
# # clear rows with all na (shouldn't be needed)
# varsImp.s <- varsImp.s[!rowSums(is.na(varsImp.s[,2:ncol(varsImp.s)]))==3,]
# # standardize to 0-1 then sort by mean importance
# for(algo in ensemble_algos){
#   naLocs <- is.na(varsImp.s[,algo])
#   varsImp.s[,algo][!naLocs] <- varsImp.s[,algo][!naLocs]/max(varsImp.s[,algo][!naLocs])
# }
# mnImp <- apply(varsImp.s[,2:ncol(varsImp.s)],1, "mean", na.rm = TRUE)
# varsImp.s <- varsImp.s[order(mnImp),]
# varsImp.s$fullName <- factor(varsImp.s$fullName, levels = varsImp.s$fullName)
# # set up the ggplot
# impPlot <- ggplot(data = varsImp.s) + 
#   xlab(bquote(atop("lower" %->% "greater", "importance"))) + 
#   theme(axis.title.y = element_blank(),
#         text = element_text(size=8),
#         legend.position = c(0.85,0.15))
# # loop through algos, add each to the plot, use same colors as in ROC plot
# for(algo in ensemble_algos){
#   specs <- figSpecs[match(algo,figSpecs$algos),] 
#   inString <- paste("geom_point(aes(x = ", algo, ", y = fullName), color = '", specs$col, "', na.rm = TRUE)")
#   impPlot <- impPlot + eval(parse(text = inString))
#   inString <- paste("geom_path(data = varsImp.s[!is.na(varsImp.s[algo]),],",
#                     " aes(x = ", algo, ", y = as.numeric(fullName)), color = '", specs$col, "', na.rm = TRUE)")
#   impPlot <- impPlot + eval(parse(text = inString))
# }

## do it in long format !!
# standardize to 0-1 then sort by mean importance (using factors)
for(algo in ensemble_algos){
  algoLocs <- varsImp$algorithm == algo
  varsImp[algoLocs,"impVal"] <- varsImp[algoLocs, "impVal"]/max(varsImp[algoLocs,"impVal"])
}

# varsSorted <- varsImp %>%
#   group_by(fullName) %>%
#   summarise_at(vars(impVal), mean) %>%
#   arrange(impVal)

#try sorting with zeros added
varsSorted <- varsImp %>%
  group_by(fullName) %>%
  summarise_at(vars(impVal), function(x) { sum(x)/length(ensemble_algos)}) %>%
  arrange(impVal)



varsImp$fullName <- factor(varsImp$fullName, levels = varsSorted$fullName)
varsImp <- varsImp[order(as.integer(varsImp$fullName)),]

#use same colors as ROC plot
scaleVec <- figSpecs$col
names(scaleVec) <- figSpecs$algos

# build the figure 
impPlot <- ggplot(data = varsImp) + 
  xlab(bquote(atop("lower" %->% "greater", "importance"))) + 
  theme(axis.title.y = element_blank(),
        text = element_text(size=8),
        legend.position = c(0.85,0.15)) + 
  geom_point(aes(x = impVal, y = fullName, color = algorithm)) + 
  geom_path(aes(x = impVal, y = fullName, color = algorithm, group = algorithm)) + 
  scale_color_manual(values = scaleVec)


# ## get background poly data for the map (study area, reference boundaries) ----
# studyAreaExtent <- st_read(here("_data","species",model_species,"inputs","model_input",paste0(model_run_name, "_studyArea.gpkg")), quiet = T)
# referenceBoundaries <- st_read(nm_refBoundaries, quiet = T) # name of state boundaries file
# 
# r <- dir(path = "model_predictions", pattern = ".tif$",full.names=FALSE)
# fileName <- r[grep(model_run_name, r)]
# fileName <- fileName[[1]] #only take the first for now TODO: handle ensembles
# 
# ras <- raster(paste0("model_predictions/", fileName))
# 
# # project to match raster, just in case
# studyAreaExtent <- st_transform(studyAreaExtent, as.character(ras@crs))
# referenceBoundaries <- st_transform(referenceBoundaries, as.character(ras@crs))

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
SQLquery <- paste("Select ElemCode, dateTime, cutCode, cutValue, capturedEOs, capturedPts ", 
                  "FROM tblModelResultsCutoffs ", 
                  "WHERE model_run_name ='", model_run_name, "'; ", sep="")
sdm.thresholds <- dbGetQuery(db, statement = SQLquery)
# filter to only most recent
#uniqueTimes <- unique(sdm.thresholds$dateTime)
#mostRecent <- uniqueTimes[order(uniqueTimes, decreasing = TRUE)][[1]]
#sdm.thresholds <- sdm.thresholds[sdm.thresholds$dateTime == mostRecent,]

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
sdm.thresh.table <- sdm.thresh.merge[,c("cutFullName", "cutValue",
  "capturedEOs", "capturedPts", "cutDescription")]
names(sdm.thresh.table) <- c("Threshold", "Value", "Groups","Pts","Description")
sdm.thresh.table$Groups <- paste(round(sdm.thresh.table$Groups/inputs$feat_grp_count[[1]]*100, 1),
                                     "(",sdm.thresh.table$Groups, ")", sep="")
# sdm.thresh.table$Polys <- paste(round(sdm.thresh.table$Polys/numPys*100, 1),
#                               "(",sdm.thresh.table$Polys, ")", sep="")
#numPts <- nrow(subset(df.full, pres == 1))
sdm.thresh.table$Pts <- paste(round(sdm.thresh.table$Pts/inputs$feat_count[[1]]*100, 1),
                              sep="")

## get grank definition ----
SQLquery <- paste0("SELECT rank, rankname FROM lkpRankDefinitions where rank = '",ElementNames$rounded_g_rank,"';", sep="")
grank_desc <- dbGetQuery(db, SQLquery)

# make a url to NatureServe Explorer
NSurl <- paste("http://explorer.natureserve.org/servlet/NatureServe?searchName=",gsub(" ", "+", ElementNames[[1]], fixed=TRUE), sep="")

## get Model Evaluation and Use data ----
SQLquery <- paste("Select spdata_dataqual, spdata_abs, spdata_eval, envvar_relevance, envvar_align, process_algo, process_sens, process_rigor, process_perform, process_review, products_mapped, products_support, products_repo, iterative, spdata_dataqualNotes, spdata_absNotes, spdata_evalNotes, envvar_relevanceNotes, envvar_alignNotes, process_algoNotes, process_sensNotes, process_rigorNotes, process_performNotes, process_reviewNotes, products_mappedNotes, products_supportNotes, products_repoNotes, iterativeNotes ", 
                  "FROM lkpSpeciesRubric ", 
                  "WHERE sp_code ='", model_species, "'; ", sep="")
sdm.modeluse <- dbGetQuery(db, statement = SQLquery)
sdm.modeluse[is.na(sdm.modeluse)] <- " "
sdm.modeluse[sdm.modeluse=="I"] <- "\\cellcolor[HTML]{9AFF99} Ideal"
sdm.modeluse[sdm.modeluse=="A"] <- "\\cellcolor[HTML]{FFFFC7} Acceptable"
sdm.modeluse[sdm.modeluse=="C"] <- "\\cellcolor[HTML]{FD6864} Interpret with Caution"

## Get env. var lookup table ----
SQLquery <- paste0("SELECT gridName g from tblModelResultsVarsUsed where model_run_name = '",
                   model_run_name, "' and inFinalModel = 1;")
var_names <- dbGetQuery(db, SQLquery)$g
SQLquery <- paste("SELECT fullName, description ",
                  "FROM lkpEnvVars ",
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

# writing to the same folder as a grid might cause problems.
# if errors check that first
#   more explanation: tex looks for and uses aux files, which are also used
#   by esri. If there's a non-tex aux file, knitr bails. 

# Also, might need to run this twice. First time through tex builds the reference
# list, second time through it can then number the refs in the doc.

setwd("metadata")
# knit2pdf errors for some reason...just knit then call directly
#knit(paste(loc_scripts,"MetadataEval_knitr.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
knit2pdf(paste(loc_scripts,"MetadataEval_knitr_test.rnw",sep="/"), output=paste(model_run_name, ".tex",sep=""))
#call <- paste0("pdflatex -interaction=nonstopmode ",model_run_name , ".tex")
# call <- paste0("pdflatex -halt-on-error -interaction=nonstopmode ",model_run_name , ".tex") # this stops execution if there is an error. Not really necessary
#system(call)
#system(call) # 2nd run to apply citation numbers

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
