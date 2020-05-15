# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(ROCR)
library(RSQLite)
library(DBI)
library(xgboost)

### find and load model data ----
# setwd(loc_model)
# setwd(paste0(model_species,"/outputs"))
# load(paste0("rdata/",modelrun_meta_data$model_run_name,".Rdata"))

## Calculate different thresholds ----
#set an empty list
cutList <- list()

# total number of EOs, polys, pts (subtract absence class)
totEOs.s <- length(unique(df.full.s$group_id)[!grepl("pseu-a",unique(df.full.s$group_id))])
totPolys.s <- length(unique(df.full.s$stratum)[!grepl("pseu-a",unique(df.full.s$stratum))])
totPts.s <- nrow(df.full.s[df.full.s$pres == 1,])

#get minimum training presence
# the 'handle' might go awry
xgb.full <- xgb.Booster.complete(xgb.full)

#needs to be re-created ... contains an external pointer 
df.full.s.xgb <- xgb.DMatrix(as.matrix(df.full.s[,indVarCols]), 
                             label=as.integer(as.character(df.full.s$pres)))

xgb.predicted <- predict(xgb.full, df.full.s.xgb)
xgb.predicted <- as.data.frame(cbind(df.full.s[,c("pres","group_id","stratum")], "pred" = xgb.predicted))
allVotesPresPts <- xgb.predicted[xgb.predicted$pres=="1",]
#mtp
MTP <- min(allVotesPresPts[, "pred"])
capturedEOs <- length(unique(allVotesPresPts$group_id))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapEOs"= propCaptEOs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

#get 10 percentile training presence
TenPctile <- quantile(allVotesPresPts$pred, prob = c(0.1))
TenPctilePts <- allVotesPresPts[allVotesPresPts$pred >= TenPctile,]
capturedEOs <- length(unique(TenPctilePts$group_id))
capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                          "capturedEOs" = capturedEOs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts,
                          "prpCapEOs"= propCaptEOs,
                          "prpCapPolys" = propCaptPolys,
                          "prpCapPts"=  propCaptPts)

# # get min of max values by polygon (MTPP; minimum training polygon presence)
# maxInEachPoly <- aggregate(allVotesPresPts$X1, 
#                            by=list(allVotesPresPts$stratum, allVotesPresPts$group_id), max)
# names(maxInEachPoly) <- c("stratum","group_id","X1")
# MTPP <- min(maxInEachPoly$X1)
# capturedEOs <- length(unique(maxInEachPoly$group_id))
# capturedPolys <- length(unique(maxInEachPoly$stratum))
# capturedPts <- nrow(allVotesPresPts[allVotesPresPts$X1 >= MTPP,])
# cutList$MTPP <- list("value" = MTPP, "code" = "MTPP", 
#                     "capturedEOs" = capturedEOs,
#                     "capturedPolys" = capturedPolys,
#                     "capturedPts" = capturedPts)

# get min of max values by EO (MTPEO; minimum training EO presence)
maxInEachEO <- aggregate(allVotesPresPts$pred, 
                           by=list(allVotesPresPts$group_id), max)
names(maxInEachEO) <- c("group_id","pred")
MTPEO <- min(maxInEachEO$pred)
capturedEOs <- length(unique(maxInEachEO$group_id))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MTPEO,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$pred >= MTPEO,])
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$MTPEO <- list("value" = MTPEO, "code" = "MTPEO", 
                      "capturedEOs" = capturedEOs,
                      "capturedPolys" = capturedPolys,
                      "capturedPts" = capturedPts,
                      "prpCapEOs"= propCaptEOs,
                      "prpCapPolys" = propCaptPolys,
                      "prpCapPts"=  propCaptPts)


# F-measure cutoff skewed towards capturing more presence points.
# extract the precision-recall F-measure from training data
# set alpha very low to tip in favor of 'presence' data over 'absence' data
# based on quick assessment in Spring 07, set alpha to 0.01
alph <- 0.01
#create the prediction object for ROCR. Get pres col from votes (=named "1")
xgb.full.rocr.pred <- ROCR::prediction(xgb.predicted$pred,xgb.predicted$pres)
#use ROCR performance to get the f measure
xgb.full.f <- ROCR::performance(xgb.full.rocr.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
xgb.full.f.df <- data.frame(cutoff = unlist(xgb.full.f@x.values),fmeasure = unlist(xgb.full.f@y.values))
xgb.full.ctoff <- c(1-xgb.full.f.df[which.max(xgb.full.f.df$fmeasure),][["cutoff"]], xgb.full.f.df[which.max(xgb.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(xgb.full.ctoff) <- c("0","1")
FMeasPt01 <- xgb.full.ctoff[2]
z <- allVotesPresPts[allVotesPresPts$pred >= FMeasPt01,]
capturedEOs <- length(unique(z$group_id))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedEOs" = capturedEOs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts,
                          "prpCapEOs"= propCaptEOs,
                          "prpCapPolys" = propCaptPolys,
                          "prpCapPts"=  propCaptPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
xgb.full.sens <- performance(xgb.full.rocr.pred,"sens")
xgb.full.spec <- performance(xgb.full.rocr.pred,"spec")
xgb.full.sss <- data.frame(cutSens = unlist(xgb.full.sens@x.values),sens = unlist(xgb.full.sens@y.values),
                          cutSpec = unlist(xgb.full.spec@x.values), spec = unlist(xgb.full.spec@y.values))
xgb.full.sss$sss <- with(xgb.full.sss, sens + spec)
maxSSS <- xgb.full.sss[which.max(xgb.full.sss$sss),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$pred >= maxSSS,]
capturedEOs <- length(unique(z$group_id))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
                       "capturedEOs" = capturedEOs,
                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts,
                       "prpCapEOs"= propCaptEOs,
                       "prpCapPolys" = propCaptPolys,
                       "prpCapPts"=  propCaptPts)

#equal sensitivity and specificity
xgb.full.sss$diff <- abs(xgb.full.sss$sens - xgb.full.sss$spec)
eqss <- xgb.full.sss[which.min(xgb.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$pred >= eqss,]
capturedEOs <- length(unique(z$group_id))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                     "capturedEOs" = capturedEOs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapEOs"= propCaptEOs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)

# upper left corner of ROC plot
xgb.full.perf <- performance(xgb.full.rocr.pred, "tpr","fpr")
# use pythagorean formula to get hypotenuse distance from 0,1 to each point in curve
dist.to.01 <- sqrt(xgb.full.perf@x.values[[1]]^2 + (1-xgb.full.perf@y.values[[1]])^2)
ROCupperleft <- xgb.full.perf@alpha.values[[1]][[which.min(dist.to.01)]]
z <- allVotesPresPts[allVotesPresPts$pred >= ROCupperleft,]
capturedEOs <- length(unique(z$group_id))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptEOs <- capturedEOs/totEOs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapEOs"= propCaptEOs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

#calc a few measures on the full set, call it test set
# total number of EOs (subtract absence class)
totEOs.f <- length(unique(df.full$group_id)[!grepl("pseu-a",unique(df.full$group_id))])
totPolys.f <- length(unique(df.full$stratum)[!grepl("pseu-a",unique(df.full$stratum))])
totPts.f <- nrow(df.full[df.full$pres == 1,])


#needs to be re-created ... contains an external pointer 
df.full.xgb <- xgb.DMatrix(as.matrix(df.full[,indVarCols]), 
                             label=as.integer(as.character(df.full$pres)))

xgb.predicted <- predict(xgb.full, df.full.xgb)
xgb.predicted <- as.data.frame(cbind(df.full[,c("pres","group_id","stratum")], "pred" = xgb.predicted))
allVotesPresPts <- xgb.predicted[xgb.predicted$pres=="1",]

#get minimum test set presence "Min Pres Validation Points"
MPVP <- min(allVotesPresPts[, "pred"])
capturedEOs <- length(unique(allVotesPresPts$group_id))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
propCaptEOs <- capturedEOs/totEOs.f
propCaptPolys <- capturedPolys/totPolys.f
propCaptPts <- capturedPts/totPts.f
cutList$MPVP <- list("value" = MPVP, "code" = "MPVP", 
                     "capturedEOs" = capturedEOs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapEOs"= propCaptEOs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)

#get minimum test set presence by group "Min Pres Validation Groups"
#use polys for terrestrial models (EOs would be the same as subset)
maxInEachGP <- aggregate(allVotesPresPts$pred, 
                         by=list(allVotesPresPts$stratum), max)
names(maxInEachGP) <- c("stratum","pred")
MPVG <- min(maxInEachGP$pred)
capturedEOs <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MPVG,"group_id"]))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MPVG,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$pred >= MPVG,])
propCaptEOs <- capturedEOs/totEOs.f
propCaptPolys <- capturedPolys/totPolys.f
propCaptPts <- capturedPts/totPts.f
cutList$MPVG <- list("value" = MPVG, "code" = "MPVG", 
                     "capturedEOs" = capturedEOs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapEOs"= propCaptEOs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)



# collate and write to DB ----

# number of thresholds to write to the db
numThresh <- length(cutList)

allThresh <- data.frame("model_run_name" = rep(modelrun_meta_data$model_run_name, numThresh),
                        "algorithm" = rep(algo, numThresh),
                        "ElemCode" = rep(ElementNames$Code, numThresh),
                        "dateTime" = rep(as.character(Sys.time()), numThresh),
                        "cutCode" = unlist(lapply(cutList, function(x) x["code"])),
                        "cutValue" = unlist(lapply(cutList, function(x) x["value"])),
                        "capturedEOs" = unlist(lapply(cutList, function(x) x["capturedEOs"])),
                        "capturedPolys" = unlist(lapply(cutList, function(x) x["capturedPolys"])),
                        "capturedPts" = unlist(lapply(cutList, function(x) x["capturedPts"])),
                        "prpCapEOs" = unlist(lapply(cutList, function(x) x["prpCapEOs"])),
                        "prpCapPolys" = unlist(lapply(cutList, function(x) x["prpCapPolys"])),
                        "prpCapPts" = unlist(lapply(cutList, function(x) x["prpCapPts"])),
                        stringsAsFactors = FALSE)

db <- dbConnect(SQLite(),dbname=nm_db_file)
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)

dbWriteTable(db, "tblModelResultsCutoffs", allThresh, append = TRUE)
# clean up
options(op)
dbDisconnect(db)

