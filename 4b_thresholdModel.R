# File: 4b_thresholdModel.r
# Purpose: threshold the probabilities in the results shapefile

## start with a fresh workspace with no objects loaded
library(sf)
library(ROCR)
library(RSQLite)
library(DBI)

### find and load model data ----
setwd(loc_model)
setwd(paste0(model_species,"/outputs"))
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

## Calculate different thresholds ----
#set an empty list
cutList <- list()

#get minimum training presence
#allVotes <- data.frame(rf.full$y, rf.full$votes, df.full[,c("group_id", "stratum")])
allVotes <- data.frame(y = rf.full$y, rf.full$votes, group_id = df.full[,group$colNm])
allVotesPresPts <- allVotes[allVotes$y==1,]
# edge cases, remove NaN from allvotesPresPts
allVotesPresPts <- allVotesPresPts[complete.cases(allVotesPresPts),]

MTP <- min(allVotesPresPts$X1, na.rm = FALSE)
capturedEOs <- length(unique(allVotesPresPts$group_id))  
###capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedEOs" = capturedEOs,
                    #"capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

#get 10 percentile training presence
TenPctile <- quantile(allVotesPresPts$X1, prob = c(0.1), na.rm = FALSE)
TenPctilePts <- allVotesPresPts[allVotesPresPts$X1 >= TenPctile,]
capturedEOs <- length(unique(TenPctilePts$group_id))
#capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                    "capturedEOs" = capturedEOs,
#                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

# get MTPG (by group)
MTPG <- min(aggregate(allVotesPresPts$X1, by=list(allVotesPresPts$group_id), FUN = max)$x)
MTPGPts <- allVotesPresPts[allVotesPresPts$X1 >= MTPG,]
capturedEOs <- length(unique(allVotesPresPts$group_id))
capturedPts <- nrow(MTPGPts)
cutList$MTPG <- list("value"=MTPG, "code"="MTPG", "capturedEOs"=capturedEOs, "capturedPts"=capturedPts)

# F-measure cutoff skewed towards capturing more presence points.
# extract the precision-recall F-measure from training data
# set alpha very low to tip in favor of 'presence' data over 'absence' data
# based on quick assessment in Spring 07, set alpha to 0.01
alph <- 0.01
#create the prediction object for ROCR. Get pres col from votes (=named "1")
rf.full.pred <- prediction(rf.full$votes[,"1"],df.full$pres)
#use ROCR performance to get the f measure
rf.full.f <- performance(rf.full.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
rf.full.f.df <- data.frame(cutoff = unlist(rf.full.f@x.values),fmeasure = unlist(rf.full.f@y.values))
rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(rf.full.ctoff) <- c("0","1")
FMeasPt01 <- rf.full.ctoff[2]
z <- allVotesPresPts[allVotesPresPts$X1 >= FMeasPt01,]
capturedEOs <- length(unique(z$group_id))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedEOs" = capturedEOs,
#                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
rf.full.sens <- performance(rf.full.pred,"sens")
rf.full.spec <- performance(rf.full.pred,"spec")
rf.full.sss <- data.frame(cutSens = unlist(rf.full.sens@x.values),sens = unlist(rf.full.sens@y.values),
                          cutSpec = unlist(rf.full.spec@x.values), spec = unlist(rf.full.spec@y.values))
rf.full.sss$sss <- with(rf.full.sss, sens + spec)
maxSSS <- rf.full.sss[which.max(rf.full.sss$sss),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= maxSSS,]
capturedEOs <- length(unique(z$group_id))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
  "capturedEOs" = capturedEOs,
#  "capturedPolys" = capturedPolys,
  "capturedPts" = capturedPts)

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= eqss,]
capturedEOs <- length(unique(z$group_id))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                       "capturedEOs" = capturedEOs,
#                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts)

# upper left corner of ROC plot
### NO, it looks like these calculations are technically not the upper left corner
### and the upper left corner is different from these others.
### So this needs reworking to represent ROC if it is ever used. Need to use
### pythagorean formula, I think. Liu etal 2005 and Cantor et al 1999 don't seem 
### to provide formula. Aha, see package OptimalCutpoints
# rf.full.perf <- performance(rf.full.pred, "tpr","fpr")
# cutpt <- which.max(abs(rf.full.perf@x.values[[1]]-rf.full.perf@y.values[[1]]))
# ROCupperleft <- rf.full.perf@alpha.values[[1]][cutpt]
# z <- allVotesPresPts[allVotesPresPts$X1 >= ROCupperleft,]
# capturedEOs <- length(unique(z$group_id))
# capturedPolys <- length(unique(z$stratum))
# capturedPts <- nrow(z)
# cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
#                           "capturedEOs" = capturedEOs,
#                           "capturedPolys" = capturedPolys,
#                           "capturedPts" = capturedPts)

# collate and write to DB ----

# load the prediction vector, for calculating MRV
#results_shape <- st_read(paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results.shp"), quiet = T)

# MRV (on full model predictions)
###pres.comid <- df.full$comid[df.full$pres==1]
###mrv <- min(results_shape$prbblty[results_shape$comid %in% pres.comid], na.rm = T)
##cutList$MRV <- list("value" = mrv, "code" = "MRV", "capturedPts" = length(pres.comid))

# number of thresholds to write to the db
numThresh <- length(cutList)

allThresh <- data.frame("model_run_name" = rep(modelrun_meta_data$model_run_name, numThresh),
                        "ElemCode" = rep(ElementNames$Code, numThresh),
                "dateTime" = rep(as.character(Sys.time()), numThresh),
                "cutCode" = unlist(lapply(cutList, function(x) x[2])),
                "cutValue" = unlist(lapply(cutList, function(x) x[1])),
                "capturedEOs" = unlist(lapply(cutList, function(x) x[3])),
#                "capturedPolys" = unlist(lapply(cutList, function(x) x[4])),
                "capturedPts" = unlist(lapply(cutList, function(x) x[4])),
                stringsAsFactors = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

# add a check to see if the data is populated and skip writing to the table if there are any rows for the model run name.
dbcheck <- dbGetQuery(db, paste0("SELECT cutCode c FROM tblModelResultsCutoffs WHERE model_run_name = '",
                                 modelrun_meta_data$model_run_name,"';"))$c
if(length(dbcheck)==0){
  dbWriteTable(db, "tblModelResultsCutoffs", allThresh, append = T)  
}


# clean up
options(op)
dbDisconnect(db)

# write the thresholds to the line shapefile
results_shape <- st_read(paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results.shp"), quiet = T)
# THE next lines are for creating threshold column(s) in the shapefile
for (t in allThresh$cutCode) { # add columns for all thresholds
  threshold <- as.numeric(allThresh$cutValue[allThresh$cutCode == t])
  if (!is.na(threshold)) {
    # reclassify the vector based on the threshold into binary 0/1
    results_shape[,t] <- NA
    results_shape[,t] <- ifelse(results_shape$prbblty < threshold, 0, 1)
  }
}
st_write(results_shape, paste0("model_predictions/",modelrun_meta_data$model_run_name, "_results.shp"), delete_layer = T) #write out shapefile

# write the thresholds to the aqua polys shapefile
if(file.exists(paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results_aquaPolys.shp"))){
  results_shape <- st_read(paste0("model_predictions/", modelrun_meta_data$model_run_name, "_results_aquaPolys.shp"), quiet = T)
  # THE next lines are for creating threshold column(s) in the shapefile  
  for (t in allThresh$cutCode) { # add columns for all thresholds
    threshold <- as.numeric(allThresh$cutValue[allThresh$cutCode == t])
    if (!is.na(threshold)) {
      # reclassify the vector based on the threshold into binary 0/1
      results_shape[,t] <- NA
      results_shape[,t] <- ifelse(results_shape$prbblty < threshold, 0, 1)
    }
  }
  st_write(results_shape, paste0("model_predictions/",modelrun_meta_data$model_run_name, "_results_aquaPolys.shp"), delete_layer = T) #write out shapefile 
} else {
  cat("No aquatic polygon shapefile, skipping tresholding it.\n")
}


#clean up
rm(results_shape)
