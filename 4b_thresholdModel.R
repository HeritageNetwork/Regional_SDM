# File: 4b_thresholdModel.r
# Purpose: threshold the distribution model prediction raster

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)
library(ROCR)
library(RSQLite)
library(DBI)

inPath <- "K:/Reg5Modeling_Project/outputs"
gridpath <- "K:/Reg5Modeling_Project/outputs/grids"
#out path
outRas <- "K:/Reg5Modeling_Project/outputs/grids" 

dbLoc <- "G:/RegionalSDM/databases"

## get any current documentation ----
db_file <- paste(dbLoc, "SDM_lookupAndTracking.sqlite", sep = "/")

## find and load model data ----
# get a list of what's in the directory
d <- dir(path = inPath, pattern = ".Rdata",full.names=FALSE)
d
# which one do we want to run?
n <- 4
fileName <- d[[n]]
load(paste(inPath,fileName, sep="/"))

## Calculate different thresholds ----
#set an empty list
cutList <- list()

# total number of EOs (subtract absence class)
totEOs <- length(unique(df.full$eo_id_st)) - 1
# total number of polys
totPolys <- length(unique(df.full$stratum)) - 1

#get minimum training presence
x <- data.frame(rf.full$y, rf.full$votes, df.full[,c("eo_id_st", "stratum")])
y <- x[x$rf.full.y ==1,]
MTP <- min(y$X1)
capturedEOs <- length(unique(y$eo_id_st))
capturedPolys <- length(unique(y$stratum))
capturedPts <- nrow(y)
cutList$MTP <- list("value" = MTP, "code" = "MTP", 
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
                    "capturedPts" = capturedPts)

#get 10 percentile training presence
TenPctile <- quantile(y$X1, prob = c(0.1))
z <- y[y$X1 >= TenPctile,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$TenPctile <- list("value" = TenPctile, "code" = "TenPctile",
                    "capturedEOs" = capturedEOs,
                    "capturedPolys" = capturedPolys,
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
z <- y[y$X1 >= FMeasPt01,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$FMeasPt01 <- list("value" = FMeasPt01, "code" = "FMeasPt01",
                          "capturedEOs" = capturedEOs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts)

#max sensitivity plus specificity (maxSSS per Liu et al 2016)
rf.full.sens <- performance(rf.full.pred,"sens")
rf.full.spec <- performance(rf.full.pred,"spec")
rf.full.sss <- data.frame(cutSens = unlist(rf.full.sens@x.values),sens = unlist(rf.full.sens@y.values),
                          cutSpec = unlist(rf.full.spec@x.values), spec = unlist(rf.full.spec@y.values))
rf.full.sss$sss <- with(rf.full.sss, sens + spec)
maxSSS <- rf.full.sss[which.max(rf.full.sss$sss),"cutSens"]
z <- y[y$X1 >= maxSSS,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$maxSSS <- list("value" = maxSSS, "code" = "maxSSS",
  "capturedEOs" = capturedEOs,
  "capturedPolys" = capturedPolys,
  "capturedPts" = capturedPts)

#equal sensitivity and specificity
rf.full.sss$diff <- abs(rf.full.sss$sens - rf.full.sss$spec)
eqss <- rf.full.sss[which.min(rf.full.sss$diff),"cutSens"]
z <- y[y$X1 >= eqss,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$eqss <- list("value" = eqss, "code" = "eqSS",
                       "capturedEOs" = capturedEOs,
                       "capturedPolys" = capturedPolys,
                       "capturedPts" = capturedPts)

# upper left corner of ROC plot
rf.full.perf <- performance(rf.full.pred, "tpr","fpr")
cutpt <- which.max(abs(rf.full.perf@x.values[[1]]-rf.full.perf@y.values[[1]]))
ROCupperleft <- rf.full.perf@alpha.values[[1]][cutpt]
z <- y[y$X1 >= ROCupperleft,]
capturedEOs <- length(unique(z$eo_id_st))
capturedPolys <- length(unique(z$stratum))
capturedPts <- nrow(z)
cutList$ROC <- list("value" = ROCupperleft, "code" = "ROC",
                          "capturedEOs" = capturedEOs,
                          "capturedPolys" = capturedPolys,
                          "capturedPts" = capturedPts)

##auc <- performance(rf.full.pred, "auc")

# collate and write to DB ----

# number of thresholds to write to the db
numThresh <- length(cutList)

allThresh <- data.frame("ElemCode" = rep(ElementNames$Code, numThresh),
                "dateTime" = rep(as.character(Sys.time()), numThresh),
                "cutCode" = unlist(lapply(cutList, function(x) x[2])),
                "cutValue" = unlist(lapply(cutList, function(x) x[1])),
                "capturedEOs" = unlist(lapply(cutList, function(x) x[3])),
                "capturedPolys" = unlist(lapply(cutList, function(x) x[4])),
                "capturedPts" = unlist(lapply(cutList, function(x) x[5])),
                stringsAsFactors = FALSE)

db <- dbConnect(SQLite(),dbname=db_file)


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
#lets set the threshold to MTP
threshold <- allThresh$MTP

# load the prediction grid
ras <- raster(paste(gridpath,"/",ElementNames$Code, ".tif", sep = ""))

# reclassify the raster based on the threshold into binary 0/1
m <- cbind(
  from = c(-Inf, threshold),
  to = c(threshold, Inf),
  becomes = c(0, 1)
)

rasrc <- reclassify(ras, m)

#plot(rasrc)
outfile <- paste(outRas,"/",ElementNames$Code,"_threshold.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)

#clean up
rm(m, rasrc)

## continuous grid that drops cells below thresh ----
# reclassify the raster based on the threshold into Na below thresh
m <- cbind(
  from = c(-Inf),
  to = c(threshold),
  becomes = c(NA)
)

rasrc <- reclassify(ras, m)

#plot(rasrc)
outfile <- paste(outRas,"/",ElementNames$Code,"_thresh2.tif", sep = "")
writeRaster(rasrc, filename=outfile, format="GTiff", overwrite=TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())
