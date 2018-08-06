# File: 4b_thresholdModel.r
# Purpose: threshold the probabilities in the results shapefile

## start with a fresh workspace with no objects loaded
library(rgdal)
library(ROCR)
library(RSQLite)
library(DBI)

### find and load model data ----
## two lines need your attention. The one directly below (loc_scripts)
## and about line 23 where you choose which Rdata file to use
setwd(loc_modelOut)
load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

## Calculate different thresholds ----
#set an empty list
cutList <- list()

#get minimum training presence
allVotes <- data.frame(rf.full$y, rf.full$votes, df.full[,c("eo_id_st", "stratum")])
allVotesPresPts <- allVotes[allVotes$rf.full.y ==1,]

# na.rm = TRUE for testing
MTP <- min(allVotesPresPts$X1, na.rm = FALSE)
###capturedEOs <- length(unique(allVotesPresPts$eo_id_st))  # only captured points, not EO and poly
###capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    #"capturedEOs" = capturedEOs,
                    #"capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

#get 10 percentile training presence
TenPctile <- quantile(allVotesPresPts$X1, prob = c(0.1), na.rm = FALSE)
TenPctilePts <- allVotesPresPts[allVotesPresPts$X1 >= TenPctile,]
#capturedEOs <- length(unique(TenPctilePts$eo_id_st))
#capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
#                    "capturedEOs" = capturedEOs,
#                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

# get MTPG (by group)
MTPG <- min(aggregate(allVotesPresPts$X1, by = list(allVotesPresPts$stratum), FUN = max)$x)
MTPGPts <- allVotesPresPts[allVotesPresPts$X1 >= MTPG,]
capturedPts <- nrow(MTPGPts)
cutList$MTPG <- list("value" = MTPG, "code" = "MTPG", 
                     "capturedPts" = capturedPts)

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
#capturedEOs <- length(unique(z$eo_id_st))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
#                          "capturedEOs" = capturedEOs,
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
#capturedEOs <- length(unique(z$eo_id_st))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
#  "capturedEOs" = capturedEOs,
#  "capturedPolys" = capturedPolys,
  "capturedPts" = capturedPts)

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= eqss,]
#capturedEOs <- length(unique(z$eo_id_st))
#capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
#                       "capturedEOs" = capturedEOs,
#                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts)

# upper left corner of ROC plot
### this is the same as maxSSS (pretty sure), so commented out for now
# rf.full.perf <- performance(rf.full.pred, "tpr","fpr")
# cutpt <- which.max(abs(rf.full.perf@x.values[[1]]-rf.full.perf@y.values[[1]]))
# ROCupperleft <- rf.full.perf@alpha.values[[1]][cutpt]
# z <- allVotesPresPts[allVotesPresPts$X1 >= ROCupperleft,]
# capturedEOs <- length(unique(z$eo_id_st))
# capturedPolys <- length(unique(z$stratum))
# capturedPts <- nrow(z)
# cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
#                           "capturedEOs" = capturedEOs,
#                           "capturedPolys" = capturedPolys,
#                           "capturedPts" = capturedPts)

# collate and write to DB ----

# number of thresholds to write to the db
numThresh <- length(cutList)

allThresh <- data.frame("modelRunName" = rep(modelrun_meta_data$model_run_name, numThresh),
                        "ElemCode" = rep(ElementNames$Code, numThresh),
                "dateTime" = rep(as.character(Sys.time()), numThresh),
                "cutCode" = unlist(lapply(cutList, function(x) x[2])),
                "cutValue" = unlist(lapply(cutList, function(x) x[1])),
#                "capturedEOs" = unlist(lapply(cutList, function(x) x[3])),
#                "capturedPolys" = unlist(lapply(cutList, function(x) x[4])),
                "capturedPts" = unlist(lapply(cutList, function(x) x[3])),
                stringsAsFactors = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

for(i in 1:numThresh){
  SQLquery <- paste("INSERT INTO tblCutoffs (", 
                    toString(names(allThresh)),
                    ") VALUES (",
                    toString(sQuote(allThresh[i,])),
                    ");", sep = "")
  dbExecute(db, SQLquery)
}

# clean up
options(op)
dbDisconnect(db)

## choose threshold, create binary grid ----
# THE next lines are for creating threshold column(s) in the shapefile
# you could do it in Arc instead. 

#lets set the threshold to MTP
threshold <- as.numeric(MTP)
# load the prediction vector
results_shape <- readOGR(paste0(loc_modelOut, "/model_predictions"), paste0(modelrun_meta_data$model_run_name, "_results")) # shapefile results for mapping

# reclassify the vector based on the threshold into binary 0/1
test2 <- results_shape
test2@data$MTP <- NA
test2@data$MTP[test2@data$prbblty<threshold] <- 0
test2@data$MTP[test2@data$prbblty>=threshold] <- 1

#write outshapefile
writeOGR(test2,dsn=paste0(loc_modelOut, "/model_predictions"),layer=paste0(modelrun_meta_data$model_run_name, "_results")
         , driver="ESRI Shapefile", overwrite_layer = TRUE)

#clean up
rm(test2)
