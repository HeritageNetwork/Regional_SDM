# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

library(raster)
library(sf)
library(ROCR)
library(RSQLite)
library(DBI)


## Calculate different thresholds ----
#set an empty list
cutList <- list()

#flip pres/abs back to 0/1 for consistency
gam.df.full.s$pres <- as.character(gam.df.full.s$pres)
gam.df.full.s[gam.df.full.s$pres == "abs","pres"] <- "0"
gam.df.full.s[gam.df.full.s$pres == "pres","pres"] <- "1"
gam.df.full.s$pres <- as.factor(gam.df.full.s$pres)

# total number of GPs, polys, pts (subtract absence class)
totGPs.s <- length(unique(gam.df.full.s[,group$colNm])[!grepl("pseu-a",unique(gam.df.full.s[,group$colNm]))])
totPolys.s <- length(unique(gam.df.full.s$stratum)[!grepl("pseu-a",unique(gam.df.full.s$stratum))])
totPts.s <- nrow(gam.df.full.s[gam.df.full.s$pres == "1",])

gam.predicted <- predict(gamFit1, gam.df.full.s, type = "prob")
gam.predicted <- as.data.frame(cbind(gam.df.full.s[,c("pres","group_id","stratum")], "pred" = gam.predicted[,"pres"]))
allVotesPresPts <- gam.predicted[gam.predicted$pres=="1",]

#MTP ----
MTP <- min(allVotesPresPts[, "pred"])
capturedGPs <- length(unique(allVotesPresPts[,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedGPs" = capturedGPs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapGPs"= propCaptGPs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

#get 10 percentile training presence ----
TenPctile <- quantile(allVotesPresPts$pred, prob = c(0.1))
TenPctilePts <- allVotesPresPts[allVotesPresPts$pred >= TenPctile,]
capturedGPs <- length(unique(TenPctilePts[,group$colNm]))
capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                          "capturedGPs" = capturedGPs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts,
                          "prpCapGPs"= propCaptGPs,
                          "prpCapPolys" = propCaptPolys,
                          "prpCapPts"=  propCaptPts)

# # get min of max values by polygon (MTPP; minimum training polygon presence)
# maxInEachPoly <- aggregate(allVotesPresPts$X1, 
#                            by=list(allVotesPresPts$stratum, allVotesPresPts[,group$colNm]), max)
# names(maxInEachPoly) <- c("stratum","group_id","X1")
# MTPP <- min(maxInEachPoly$X1)
# capturedGPs <- length(unique(maxInEachPoly[,group$colNm]))
# capturedPolys <- length(unique(maxInEachPoly$stratum))
# capturedPts <- nrow(allVotesPresPts[allVotesPresPts$X1 >= MTPP,])
# cutList$MTPP <- list("value" = MTPP, "code" = "MTPP", 
#                     "capturedGPs" = capturedGPs,
#                     "capturedPolys" = capturedPolys,
#                     "capturedPts" = capturedPts)

# get min of max values by GP (MTPGP; minimum training GP presence) ----
maxInEachGP <- aggregate(allVotesPresPts$pred, 
                         by=list(allVotesPresPts[,group$colNm]), max)
names(maxInEachGP) <- c(group$colNm,"pred")
MTPGP <- min(maxInEachGP$pred)
capturedGPs <- length(unique(maxInEachGP[,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MTPGP,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$pred >= MTPGP,])
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$MTPGP <- list("value" = MTPGP, "code" = "MTPGP", 
                      "capturedGPs" = capturedGPs,
                      "capturedPolys" = capturedPolys,
                      "capturedPts" = capturedPts,
                      "prpCapGPs"= propCaptGPs,
                      "prpCapPolys" = propCaptPolys,
                      "prpCapPts"=  propCaptPts)


# F-measure cutoff skewed towards capturing more presence points.----
# extract the precision-recall F-measure from training data
# set alpha very low to tip in favor of 'presence' data over 'absence' data
# based on quick assessment in Spring 07, set alpha to 0.01
alph <- 0.01
#create the prediction object for ROCR. Get pres col from votes (=named "1")
gam.full.rocr.pred <- ROCR::prediction(gam.predicted$pred,gam.predicted$pres)
#use ROCR performance to get the f measure
gam.full.f <- ROCR::performance(gam.full.rocr.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
gam.full.f.df <- data.frame(cutoff = unlist(gam.full.f@x.values),fmeasure = unlist(gam.full.f@y.values))
gam.full.ctoff <- c(1-gam.full.f.df[which.max(gam.full.f.df$fmeasure),][["cutoff"]], gam.full.f.df[which.max(gam.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(gam.full.ctoff) <- c("0","1")
FMeasPt01 <- gam.full.ctoff[2]
z <- allVotesPresPts[allVotesPresPts$pred >= FMeasPt01,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedGPs" = capturedGPs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts,
                          "prpCapGPs"= propCaptGPs,
                          "prpCapPolys" = propCaptPolys,
                          "prpCapPts"=  propCaptPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016) ----
gam.full.sens <- performance(gam.full.rocr.pred,"sens")
gam.full.spec <- performance(gam.full.rocr.pred,"spec")
gam.full.sss <- data.frame(cutSens = unlist(gam.full.sens@x.values),sens = unlist(gam.full.sens@y.values),
                           cutSpec = unlist(gam.full.spec@x.values), spec = unlist(gam.full.spec@y.values))
gam.full.sss$sss <- with(gam.full.sss, sens + spec)
maxSSS <- gam.full.sss[which.max(gam.full.sss$sss),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$pred >= maxSSS,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
                       "capturedGPs" = capturedGPs,
                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts,
                       "prpCapGPs"= propCaptGPs,
                       "prpCapPolys" = propCaptPolys,
                       "prpCapPts"=  propCaptPts)

#equal sensitivity and specificity ----
gam.full.sss$diff <- abs(gam.full.sss$sens - gam.full.sss$spec)
eqss <- gam.full.sss[which.min(gam.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$pred >= eqss,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                     "capturedGPs" = capturedGPs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapGPs"= propCaptGPs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)

# upper left corner of ROC plot ----
gam.full.perf <- performance(gam.full.rocr.pred, "tpr","fpr")
# use pythagorean formula to get hypotenuse distance from 0,1 to each point in curve
dist.to.01 <- sqrt(gam.full.perf@x.values[[1]]^2 + (1-gam.full.perf@y.values[[1]])^2)
ROCupperleft <- gam.full.perf@alpha.values[[1]][[which.min(dist.to.01)]]
z <- allVotesPresPts[allVotesPresPts$pred >= ROCupperleft,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs.s
propCaptPolys <- capturedPolys/totPolys.s
propCaptPts <- capturedPts/totPts.s
cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
                    "capturedGPs" = capturedGPs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapGPs"= propCaptGPs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

#calc a few measures on the full set, call it test set ----
# total number of GPs (subtract absence class)
totGPs.f <- length(unique(gam.df.full[,group$colNm])[!grepl("pseu-a",unique(gam.df.full[,group$colNm]))])
totPolys.f <- length(unique(gam.df.full$stratum)[!grepl("pseu-a",unique(gam.df.full$stratum))])
totPts.f <- nrow(gam.df.full[gam.df.full$pres == 1,])

gam.predicted <- predict(gamFit1, gam.df.full, type = "prob")
gam.predicted <- as.data.frame(cbind(gam.df.full[,c("pres","group_id","stratum")], "pred" = gam.predicted[,"pres"]))
allVotesPresPts <- gam.predicted[gam.predicted$pres=="1",]

#get minimum test set presence "Min Pres Validation Points" ----
MPVP <- min(allVotesPresPts[, "pred"])
capturedGPs <- length(unique(allVotesPresPts[,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
propCaptGPs <- capturedGPs/totGPs.f
propCaptPolys <- capturedPolys/totPolys.f
propCaptPts <- capturedPts/totPts.f
cutList$MPVP <- list("value" = MPVP, "code" = "MPVP", 
                     "capturedGPs" = capturedGPs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapGPs"= propCaptGPs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)

#get minimum test set presence by group "Min Pres Validation Groups" ----
#use polys for terrestrial models (GPs would be the same as subset)
maxInEachGP <- aggregate(allVotesPresPts$pred, 
                         by=list(allVotesPresPts$stratum), max)
names(maxInEachGP) <- c("stratum","pred")
MPVG <- min(maxInEachGP$pred)
capturedGPs <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MPVG,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$pred >= MPVG,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$pred >= MPVG,])
propCaptGPs <- capturedGPs/totGPs.f
propCaptPolys <- capturedPolys/totPolys.f
propCaptPts <- capturedPts/totPts.f
cutList$MPVG <- list("value" = MPVG, "code" = "MPVG", 
                     "capturedGPs" = capturedGPs,
                     "capturedPolys" = capturedPolys,
                     "capturedPts" = capturedPts,
                     "prpCapGPs"= propCaptGPs,
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
                        "capturedGPs" = unlist(lapply(cutList, function(x) x["capturedGPs"])),
                        "capturedPolys" = unlist(lapply(cutList, function(x) x["capturedPolys"])),
                        "capturedPts" = unlist(lapply(cutList, function(x) x["capturedPts"])),
                        "prpCapGPs" = unlist(lapply(cutList, function(x) x["prpCapGPs"])),
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

