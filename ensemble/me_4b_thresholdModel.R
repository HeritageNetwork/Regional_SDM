# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(ROCR)
library(RSQLite)
library(DBI)

## Calculate different thresholds ----
#set an empty list
cutList <- list()

# total number of GPs, polys, pts (subtract absence class)
totGPs.s <- length(unique(me.df.full.s[,group$colNm])[!grepl("pseu-a",unique(me.df.full.s[,group$colNm]))])
totPolys.s <- length(unique(me.df.full.s$stratum)[!grepl("pseu-a",unique(me.df.full.s$stratum))])
totPts.s <- nrow(me.df.full.s[me.df.full.s$pres == 1,])

#####
# me calculated threholds are here, for example
# me.out.fin@results[rownames(me.out.fin@results) == "Minimum.training.presence.Cloglog.threshold"]

#calc most measures on the training subset
me.predicted <- predict(me.out.fin, me.df.full.s, type = "prob")
me.predicted <- as.data.frame(cbind(me.df.full.s[,c("pres","group_id","stratum")], "pred" = me.predicted))
allVotesPresPts <- me.predicted[me.predicted$pres=="1",]

#get minimum training presence
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

#get 10 percentile training presence
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

# get min of max values by polygon (MTPP; minimum training polygon presence)
# commented for now because aquatics doesn't have two levels of groups (for now)
# maxInEachPoly <- aggregate(allVotesPresPts$pred,
#                            by=list(allVotesPresPts$stratum, allVotesPresPts[,group$colNm]), max)
# names(maxInEachPoly) <- c("stratum","group_id","pred")
# MTPP <- min(maxInEachPoly$pred)
# capturedGPs <- length(unique(maxInEachPoly[,group$colNm]))
# capturedPolys <- length(unique(maxInEachPoly$stratum))
# capturedPts <- nrow(allVotesPresPts[allVotesPresPts$pred >= MTPP,])
# cutList$MTPP <- list("value" = MTPP, "code" = "MTPP",
#                     "capturedGPs" = capturedGPs,
#                     "capturedPolys" = capturedPolys,
#                     "capturedPts" = capturedPts)

# get min of max values by GP (MTPGP; minimum training GP presence)
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


# F-measure cutoff skewed towards capturing more presence points.
# extract the precision-recall F-measure from training data
# set alpha very low to tip in favor of 'presence' data over 'absence' data
# based on quick assessment in Spring 07, set alpha to 0.01
alph <- 0.01
#create the prediction object for ROCR. Get pres col from votes (=named "1")
me.full.rocr.pred <- ROCR::prediction(me.predicted$pred,me.predicted$pres)
#use ROCR performance to get the f measure
me.full.f <- ROCR::performance(me.full.rocr.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
me.full.f.df <- data.frame(cutoff = unlist(me.full.f@x.values),fmeasure = unlist(me.full.f@y.values))
me.full.ctoff <- c(1-me.full.f.df[which.max(me.full.f.df$fmeasure),][["cutoff"]], me.full.f.df[which.max(me.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(me.full.ctoff) <- c("0","1")
FMeasPt01 <- me.full.ctoff[2]
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

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
me.full.sens <- performance(me.full.rocr.pred,"sens")
me.full.spec <- performance(me.full.rocr.pred,"spec")
me.full.sss <- data.frame(cutSens = unlist(me.full.sens@x.values),sens = unlist(me.full.sens@y.values),
                           cutSpec = unlist(me.full.spec@x.values), spec = unlist(me.full.spec@y.values))
me.full.sss$sss <- with(me.full.sss, sens + spec)
maxSSS <- me.full.sss[which.max(me.full.sss$sss),"cutSens"]
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

#equal sensitivity and specificity
me.full.sss$diff <- abs(me.full.sss$sens - me.full.sss$spec)
eqss <- me.full.sss[which.min(me.full.sss$diff),"cutSens"]
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

# upper left corner of ROC plot
me.full.perf <- performance(me.full.rocr.pred, "tpr","fpr")
# use pythagorean formula to get hypotenuse distance from 0,1 to each point in curve
dist.to.01 <- sqrt(me.full.perf@x.values[[1]]^2 + (1-me.full.perf@y.values[[1]])^2)
ROCupperleft <- me.full.perf@alpha.values[[1]][[which.min(dist.to.01)]]
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


#calc a few measures on the full set, call it test set
# total number of GPs (subtract absence class)
totGPs.f <- length(unique(me.df.full[,group$colNm])[!grepl("pseu-a",unique(me.df.full[,group$colNm]))])
totPolys.f <- length(unique(me.df.full$stratum)[!grepl("pseu-a",unique(me.df.full$stratum))])
totPts.f <- nrow(me.df.full.s[!me.df.full$stratum == "pseu-a",])

me.predicted <- predict(me.out.fin, me.df.full, type = "prob")
me.predicted <- as.data.frame(cbind(me.df.full[,c("pres","group_id","stratum")], "pred" = me.predicted))
allVotesPresPts <- me.predicted[me.predicted$pres=="1",]

#get minimum test set presence "Min Pres Validation Points"
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

#get minimum test set presence by group "Min Pres Validation Groups"
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

# first clear any results if this run has already been written to the db
sql <- paste0("Delete from tblModelResultsCutoffs WHERE model_run_name = '", 
              modelrun_meta_data$model_run_name, 
              "' AND algorithm = '",
              algo,
              "';")
dbExecute(db, statement = sql)

dbWriteTable(db, "tblModelResultsCutoffs", allThresh, append = TRUE)
# clean up
options(op)
dbDisconnect(db)
# 
# ## create all thresholds grid
# t2 <- sort(unique(allThresh$cutValue))
# t2 <- t2[!is.na(t2)]
# # get unique thresholds
# t3 <- data.frame(cutCodes = unlist(lapply(t2, FUN = function(x) {paste(allThresh$cutCode[allThresh$cutValue == x], collapse = ";")})),
#                  cutValue = t2,
#                  order = 1:length(t2))
# 
# # load the prediction grid
# ras <- raster(paste0("model_predictions/", model_run_name, ".tif"))
# 
# # reclassify the raster based on the threshold into binary 0/1
# m <- cbind(
#   from = c(-Inf, t3$cutValue),
#   to = c(t3$cutValue, Inf),
#   becomes = c(0, t3$order)
# )
# # reclassify (multi-core try)
# if (all(c("snow","parallel") %in% installed.packages())) {
#   try({
#     cat("Using multi-core processing...\n")
#     beginCluster(type = "SOCK")
#     rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
#   })
#   try(endCluster())
#   if (!exists("rasrc")) {
#     cat("Cluster processing failed. Falling back to single-core processing...\n")
#     rasrc <- reclassify(ras, m)
#   }
# } else {
#   rasrc <- reclassify(ras, m)
# }
# rasrc <- as.factor(rasrc)
# levels(rasrc) <- merge(levels(rasrc), t3, by.x = "ID", by.y = "order", all.x = T)
# 
# outfile <- paste("model_predictions/",model_run_name,"_all_thresholds.tif", sep = "")
# writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE, datatype = "INT2U")
# 
# #clean up
# rm(m, rasrc)
# 
# ## continuous grid that drops cells below lowest calculated thresh ----
# # reclassify the raster based on the threshold into Na below thresh
# m <- cbind(
#   from = c(-Inf),
#   to = min(t3$cutValue),
#   becomes = c(NA)
# )
# 
# # reclassify (multi-core try)
# if (all(c("snow","parallel") %in% installed.packages())) {
#   try({
#     cat("Using multi-core processing...\n")
#     beginCluster(type = "SOCK")
#     rasrc <- clusterR(ras, reclassify, args = list(rcl = m))
#   })
#   try(endCluster())
#   if (!exists("rasrc")) {
#     cat("Cluster processing failed. Falling back to single-core processing...\n")
#     rasrc <- reclassify(ras, m)
#   }
# } else {
#   rasrc <- reclassify(ras, m)
# }
# 
# #plot(rasrc)
# outfile <- paste("model_predictions/",model_run_name,"_min_thresh_continuous.tif", sep = "")
# writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)
