# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(ROCR)
library(RSQLite)
library(DBI)

## Calculate different thresholds ----
#set an empty list
cutList <- list()

# total number of GPs, polys, pts
totGPs <- length(unique(rf.df.full[,group$colNm])[!grepl("pseu-a",unique(rf.df.full[,group$colNm]))])
totPolys <- length(unique(rf.df.full$stratum)[!grepl("pseu-a",unique(rf.df.full$stratum))])
totPts <- nrow(rf.df.full[rf.df.full$pres == 1,])

#get minimum training presence
allVotes <- data.frame(rf.full$y, rf.full$votes/numCores, rf.df.full[,c("group_id", "stratum")])
allVotesPresPts <- allVotes[allVotes$rf.full.y ==1,]

MTP <- min(allVotesPresPts$X1)
capturedGPs <- length(unique(allVotesPresPts[,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts$stratum))
capturedPts <- nrow(allVotesPresPts)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedGPs" = capturedGPs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapGPs"= propCaptGPs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

#get 10 percentile training presence
TenPctile <- quantile(allVotesPresPts$X1, prob = c(0.1))
TenPctilePts <- allVotesPresPts[allVotesPresPts$X1 >= TenPctile,]
capturedGPs <- length(unique(TenPctilePts[,group$colNm]))
capturedPolys <- length(unique(TenPctilePts$stratum))
capturedPts <- nrow(TenPctilePts)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                    "capturedGPs" = capturedGPs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts,
                    "prpCapGPs"= propCaptGPs,
                    "prpCapPolys" = propCaptPolys,
                    "prpCapPts"=  propCaptPts)

# get min of max values by GP (MTPGP; minimum training GP presence)
maxInEachGP <- aggregate(allVotesPresPts$X1, 
                           by=list(allVotesPresPts[,group$colNm]), max)
names(maxInEachGP) <- c(group$colNm,"X1")
MTPGP <- min(maxInEachGP$X1)
capturedGPs <- length(unique(maxInEachGP[,group$colNm]))
capturedPolys <- length(unique(allVotesPresPts[allVotesPresPts$X1 >= MTPGP,"stratum"]))
capturedPts <- nrow(allVotesPresPts[allVotesPresPts$X1 >= MTPGP,])
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
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
rf.full.pred <- prediction(rf.full$votes[,"1"]/numCores,rf.df.full$pres)
#use ROCR performance to get the f measure
rf.full.f <- performance(rf.full.pred,"f",alpha = alph)
#extract the data out of the S4 object, then find the cutoff that maximize the F-value.
rf.full.f.df <- data.frame(cutoff = unlist(rf.full.f@x.values),fmeasure = unlist(rf.full.f@y.values))
rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][["cutoff"]])
#rf.full.ctoff <- c(1-rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]], rf.full.f.df[which.max(rf.full.f.df$fmeasure),][[1]])
names(rf.full.ctoff) <- c("0","1")
FMeasPt01 <- rf.full.ctoff[2]
z <- allVotesPresPts[allVotesPresPts$X1 >= FMeasPt01,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedGPs" = capturedGPs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts,
                          "prpCapGPs"= propCaptGPs,
                          "prpCapPolys" = propCaptPolys,
                          "prpCapPts"=  propCaptPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
rf.full.sens <- performance(rf.full.pred,"sens")
rf.full.spec <- performance(rf.full.pred,"spec")
rf.full.sss <- data.frame(cutSens = unlist(rf.full.sens@x.values),sens = unlist(rf.full.sens@y.values),
                          cutSpec = unlist(rf.full.spec@x.values), spec = unlist(rf.full.spec@y.values))
rf.full.sss$sss <- with(rf.full.sss, sens + spec)
maxSSS <- rf.full.sss[which.max(rf.full.sss$sss),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= maxSSS,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
  "capturedGPs" = capturedGPs,
  "capturedPolys" = capturedPolys,
  "capturedPts" = capturedPts,
  "prpCapGPs"= propCaptGPs,
  "prpCapPolys" = propCaptPolys,
  "prpCapPts"=  propCaptPts)

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]
z <- allVotesPresPts[allVotesPresPts$X1 >= eqss,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                       "capturedGPs" = capturedGPs,
                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts,
                     "prpCapGPs"= propCaptGPs,
                     "prpCapPolys" = propCaptPolys,
                     "prpCapPts"=  propCaptPts)

# upper left corner of ROC plot
rf.full.perf <- performance(rf.full.pred, "tpr","fpr")
# use pythagorean formula to get hypotenuse distance from 0,1 to each point in curve
dist.to.01 <- sqrt(rf.full.perf@x.values[[1]]^2 + (1-rf.full.perf@y.values[[1]])^2)
ROCupperleft <- rf.full.perf@alpha.values[[1]][[which.min(dist.to.01)]]
z <- allVotesPresPts[allVotesPresPts$X1 >= ROCupperleft,]
capturedGPs <- length(unique(z[,group$colNm]))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
propCaptGPs <- capturedGPs/totGPs
propCaptPolys <- capturedPolys/totPolys
propCaptPts <- capturedPts/totPts
cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
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
