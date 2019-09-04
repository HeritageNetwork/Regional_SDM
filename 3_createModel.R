# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence reaches
# - build partial plots of top performing env vars for metadata output

library(RSQLite)
library(ROCR)    #for ROC plots and stats
library(vcd)     #for kappa stats
library(abind)   #for collapsing the nested lists
library(randomForest)

source(paste0(loc_scripts, "/helper/modelrun_meta_data.R"), local = T) # generates modelrun_meta_data

setwd(loc_model)
dir.create(paste0(model_species,"/outputs/rdata"), recursive = T, showWarnings = F)
setwd(paste0("./",model_species,"/inputs"))

fileName <- paste0("model_input/", baseName, "_att.csv")

df.in <-read.csv(fileName, colClasses=c("huc12"="character"))

# absence points
fileName <- paste0("model_input/", baseName, "_bkgd_att.csv")
df.abs <- read.csv(fileName, colClasses=c("huc12"="character"))

# write model input data to database before any other changes made
db <- dbConnect(SQLite(),dbname=nm_db_file)

# set the seed before validation loops
set.seed(seed)

# get species info
SQLquery <- paste("SELECT scientific_name SciName, common_name CommName, sp_code Code, broad_group Type, egt_id, g_rank, rounded_g_rank FROM lkpSpecies WHERE sp_code = '", model_species,"';", sep="")
ElementNames <- as.list(dbGetQuery(db, statement = SQLquery)[1,])
tblModelInputs <- data.frame(table_code = baseName, EGT_ID = ElementNames$EGT_ID, datetime = as.character(Sys.time()),
                             feat_count = length(df.in$group_id), 
                             feat_grp_count = length(unique(df.in$group_id)), 
                             obs_count = length(df.in[,1]), bkgd_count = length(df.abs[,1]),
                             range_area_sqkm = NA)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append=TRUE)
# get the full list of envvars from the database
envvar_list <- dbGetQuery(db, "SELECT gridname g from lkpEnvVarsAqua;")$g
envvar_list <- tolower(envvar_list)
#also get correlated env var information
SQLquery <- "SELECT gridName, correlatedVarGroupings FROM lkpEnvVarsAqua WHERE correlatedVarGroupings IS NOT NULL order by correlatedVarGroupings;"
corrdEVs <- dbGetQuery(db, statement = SQLquery)

dbDisconnect(db)
rm(db)

# get an original list of env-vars for later writing to tblVarsUsed
envvar_list <- names(df.abs)[names(df.abs) %in% envvar_list] # gets a list of environmental variables

# strip columns with -9999  ##tgh## just a few cols. This may not be needed eventually, but needed now...
del_a_Cols <- colSums(df.abs==-9999, na.rm = TRUE)
del_a_Cols <- del_a_Cols[del_a_Cols>0]
del_p_Cols <- colSums(df.in==-9999, na.rm = TRUE)
del_p_Cols <- del_p_Cols[del_p_Cols>0]
delCols <- unique(c(names(del_a_Cols), names(del_p_Cols)))

df.abs <- df.abs[,!(names(df.abs) %in% delCols)]
df.in <- df.in[,!(names(df.in) %in% delCols)]

#make sure we don't have any NAs
df.in <- df.in[complete.cases(df.in[,!names(df.in) %in% c("obsdate","date","wacomid")]),]  # to ensure missing dates are not excluding records
df.abs <- df.abs[complete.cases(df.abs),]  # to ensure missing dates are not excluding records

# align data sets, QC ----
# add some fields to each
df.in <- cbind(df.in, pres=1, stratum=df.in$huc12)
df.abs$stratum <- "pseu-a"
df.abs <- cbind(df.abs, group_id="pseu-a", pres=0, stratum="pseu-a", SPECIES_CD="background")

# lower case column names
names(df.in) <- tolower(names(df.in))
names(df.abs) <- tolower(names(df.abs))

# are these all in the lookup database? Checking here.
db <- dbConnect(SQLite(),dbname=nm_db_file)  
op <- options("useFancyQuotes") 
options(useFancyQuotes = FALSE) #sQuote call unhappy with fancy quote, turn off
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVarsAqua WHERE LOWER(gridName) in (", 
                  toString(sQuote(envvar_list)), "); ", sep = "")
namesInDB <- dbGetQuery(db, statement = SQLquery)
namesInDB$gridName <- tolower(namesInDB$gridName)
envvar_list <- tolower(envvar_list)

## this prints rasters not in the lookup database
## if blank you are good to go, otherwise figure out what's up
envvar_list[!envvar_list %in% namesInDB$gridName]

## this prints out the rasters that don't appear as a column name
## in df.in (meaning it wasn't used to attribute or the name is funky)
## if blank you are good to go
envvar_list[!envvar_list %in% names(df.in)]

# the final envvar list is what is in the presence dataset
envvar_list <- envvar_list[envvar_list %in% names(df.in)]

#clean up
options(op)
dbDisconnect(db)
rm(db)

# clean up, merge data sets -----
# this is the full list of fields, arranged appropriately
colList <- c("species_cd","group_id","pres","stratum", "comid", "huc12", envvar_list)

# assign and calculate where the dependent and independent vars are
depVarCol <- "pres"
indVarStart <- length(colList) - length(envvar_list) + 1
indVarCols <- c(indVarStart:length(colList)) 

#re-arrange
df.in <- df.in[,colList]
df.abs <- df.abs[,colList]

# how many HUCs?
numHuc12 <- length(unique(df.in$stratum))
numHuc10 <- length(unique(substr(df.in$huc12,1,10)))
numHuc8 <- length(unique(substr(df.in$huc12,1,8)))
# how many groups based on stream grouping methods
numGps <- length(unique(df.in$group_id))

#initialize the grouping list, and set up grouping variables
group <- vector("list")
if(numHuc12 < 5){
  group$colNm <- "group_id"
  group$JackknType <- "adjacent presence reach groups"
  group$vals <- unique(df.in$group_id)
} else if(numHuc12 < 75) {
  group$colNm <- "stratum"
  group$JackknType <- "HUC 12 groups"
  group$vals <- unique(df.in$stratum)
} else if(numHuc10 < 75) {
  df.in$stratum <- substr(df.in$huc12,1,10)
  group$colNm <- "stratum"
  group$JackknType <- "HUC 10 groups"
  group$vals <- unique(df.in$stratum)
} else  {
  df.in$stratum <- substr(df.in$huc12,1,8)
  group$colNm <- "stratum"
  group$JackknType <- "HUC 8 groups"
  group$vals <- unique(df.in$stratum)
}

## groups with only one record (reach) in them are problematic
## in that they'll never be chosen as out-of-bag (OOB) samples
## because we are forcing sampling by group. If all groups in a subset from one of the validation loops
## have one record each, then the script will error out. (prediction is NaN)
## solution: lump up any group that has only one record
df.in[,group$colNm] <- as.character(df.in[,group$colNm])
groupCount <- table(df.in[,group$colNm])
if(1 %in% groupCount) {
  gpsWithOne <- groupCount[groupCount == 1]
  #merge into groups of two or three (if uneven)
  isEven <- length(gpsWithOne) %% 2 == 0
  if(length(gpsWithOne) == 1){
    #if only one group has a singleton reach, merge it with its neighbor
    mergeGroup <- names(gpsWithOne)
    mergeLocation <- grep(mergeGroup, df.in2$group_id) - 1
    if(mergeLocation == 0) mergeLocation <- 2
    targetGroup <- df.in[,group$colNm][mergeLocation]
    df.in[df.in[,group$colNm] == mergeGroup,group$colNm] <- targetGroup
    rm(mergeGroup, mergeLocation, targetGroup)
  } else if (isEven){
    targetGroups <- names(gpsWithOne)[c(1:length(gpsWithOne))[c(1:length(gpsWithOne)) %% 2 == 1]]
    mergeGroups <- names(gpsWithOne)[c(1:length(gpsWithOne))[c(1:length(gpsWithOne)) %% 2 == 0]]
    df.in[df.in[,group$colNm] %in% as.character(mergeGroups),group$colNm] <- targetGroups      
    rm(mergeGroups, targetGroups)
  } else {
    # last singleton needs to merge to final group
    targetGroups <- names(gpsWithOne)[c(1:length(gpsWithOne))[c(1:length(gpsWithOne)) %% 2 == 1]]
    mergeGroups <- names(gpsWithOne)[c(1:length(gpsWithOne))[c(1:length(gpsWithOne)) %% 2 == 0]]
    lastSingleton <- targetGroups[[length(targetGroups)]]
    lastGroup <- targetGroups[[length(targetGroups)-1]]
    targetGroups <- targetGroups[-length(targetGroups)]
    df.in[df.in[,group$colNm] %in% as.character(mergeGroups),group$colNm] <- targetGroups
    df.in[df.in[,group$colNm] %in% as.character(lastSingleton),group$colNm] <- lastGroup
    rm(mergeGroups, targetGroups, lastSingleton, lastGroup)
  }
  rm(gpsWithOne, isEven)
}
rm(groupCount)
# reset group values, needed if we dipped into the above if statement, else harmless
group$vals <- unique(df.in[,group$colNm])

# now remove absence rows with NAs
df.abs <- df.abs[complete.cases(df.abs),]

# row bind the pseudo-absences with the presence points
df.abs$group_id <- as.character(df.abs$group_id)
df.in$group_id <- as.character(df.in$group_id)
df.full <- rbind(df.in, df.abs)

# reset these factors
df.full$group_id <- factor(df.full$group_id)
df.full$pres <- factor(df.full$pres)
df.full$huc12 <- factor(df.full$huc12)
df.full$species_cd <- factor(df.full$species_cd)
df.full$stratum <- factor(df.full$stratum)

# # make sampSizeVec using assigned stratum
sampSizeVec <- table(df.full[,group$colNm]) 
sampSizeVec["pseu-a"] <- sum(sampSizeVec) - sampSizeVec["pseu-a"]  # set samples of absences equal to total presences

##
# tune mtry ----
# run through mtry twice
x <- tuneRF(df.full[,indVarCols],
            y=df.full[,depVarCol],
            ntreeTry=300, stepFactor=2, mtryStart=6,
            strata=df.full[,group$colNm], replace=TRUE, sampsize=sampSizeVec)

newTry <- x[x[,2] == min(x[,2]),1]

y <- tuneRF(df.full[,indVarCols],
            y=df.full[,depVarCol],
            ntreeTry = 300, stepFactor = 1.5, mtryStart = max(newTry),
            strata = df.full[,group$colNm], replace = TRUE, sampsize = sampSizeVec)

mtry <- max(y[y[,2] == min(y[,2]),1])
rm(x,y)

##
# Remove the least important env vars ----
##
print("Removing least important predictors")
ntrees <- 1000
rf.find.envars <- randomForest(df.full[,indVarCols],
                        y=df.full[,depVarCol],
                        importance=TRUE,
                        ntree=ntrees,
                        mtry=mtry,
                        strata = df.full[,group$colNm], replace = TRUE, sampsize = sampSizeVec) 

impvals <- importance(rf.find.envars, type = 1)
OriginalNumberOfEnvars <- length(impvals)

# first remove the bottom of the correlated vars
corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% row.names(impvals),]
for(grp in unique(corrdEVs$correlatedVarGroupings)){
  vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
  imp.sub <- impvals[rownames(impvals) %in% vars,, drop = FALSE]
  suppressWarnings(varsToDrop <- imp.sub[!imp.sub == max(imp.sub),, drop = FALSE])
  impvals <- impvals[!rownames(impvals) %in% rownames(varsToDrop),,drop = FALSE]
}
rm(vars, imp.sub, varsToDrop)

# set the percentile, here choosing above 25% percentile
envarPctile <- 0.25
y <- quantile(impvals, probs = envarPctile)
impEnvVars <- impvals[impvals > y,]
subsetNumberofEnvars <- length(impEnvVars)
rm(y)
# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% names(impEnvVars)
impEnvVarCols[1:(indVarStart-1)] <- TRUE  # use indVarStart to define where the non-env vars are
# subset!
df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(indVarStart:length(names(df.full))) # get index of env. var. columns (columns 7+)

##
# code above is for removing least important env vars
##

# prep for validation loop ----
#now that entire set is cleaned up, split back out to use any of the three DFs below
df.in2 <- subset(df.full,pres == "1")
df.abs2 <- subset(df.full, pres == "0")
# df.in2$stratum <- factor(df.in2$stratum)
# df.abs2$stratum <- factor(df.abs2$stratum)
# df.in2$huc12 <- factor(df.in2$huc12) 
# df.abs2$huc12 <- factor(df.abs2$huc12)
# df.in2$pres <- factor(df.in2$pres)
# df.abs2$pres <- factor(df.abs2$pres)

#reset the row names, needed for random subsetting method of df.abs2, below
row.names(df.in2) <- 1:nrow(df.in2)
row.names(df.abs2) <- 1:nrow(df.abs2)

##initialize the Results vectors for output from the jackknife runs
trRes <- vector("list",length(group$vals))
   names(trRes) <- group$vals[]
evSet <- vector("list",length(group$vals))
   names(evSet) <- group$vals[]	   
evRes <- vector("list",length(group$vals))
   names(evRes) <- group$vals[]
t.f <- vector("list",length(group$vals))
   names(t.f) <- group$vals[]
t.ctoff <- vector("list",length(group$vals))
   names(t.ctoff) <- group$vals[]
v.rocr.rocplot <- vector("list",length(group$vals))
   names(v.rocr.rocplot) <- group$vals[]
v.rocr.auc <- vector("list",length(group$vals))
   names(v.rocr.auc) <- group$vals[]
v.y <- vector("list",length(group$vals))
   names(v.y) <- group$vals[]
v.kappa <- vector("list",length(group$vals))
   names(v.kappa) <- group$vals[]
v.tss <- vector("list",length(group$vals))
   names(v.tss) <- group$vals[]
v.OvAc <- vector("list",length(group$vals))
   names(v.OvAc) <- group$vals[]
t.importance <- vector("list",length(group$vals))
   names(t.importance) <- group$vals[]
t.rocr.pred <- vector("list",length(group$vals))
   names(t.rocr.pred) <- group$vals[]
v.rocr.pred <- vector("list",length(group$vals))
   names(v.rocr.pred) <- group$vals[]
   
#######
## This is the validation loop. ----
## it creates a model for all-but-one group (EO, polygon, or group),
## tests if it can predict that group left out,
## then moves on to another group, cycling though all groups
## Validation stats in tabular form are the final product.
#######
      
if(length(group$vals)>1){
	for(i in 1:length(group$vals)){
		   # Create an object that stores the select command, to be used by subset.
		  trSelStr <- parse(text=paste(group$colNm[1]," != '", group$vals[[i]],"'",sep=""))
		  evSelStr <- parse(text=paste(group$colNm[1]," == '", group$vals[[i]],"'",sep=""))
		  # apply the subset. do.call is needed so selStr can be evaluated correctly
		  trSet <- do.call("subset",list(df.in2, trSelStr))
		  evSet[[i]] <- do.call("subset",list(df.in2, evSelStr))
	    # use sample to grab a random subset from the background points
		  BGsampSz <- nrow(evSet[[i]]) * 10 #* 2 # this was initially set to 10, but in many cases there aren't enough, so we moved it down to 2.
		  evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz, replace=FALSE, prob=NULL),]
		  # get the other portion for the training set
		  TrBGsamps <- attr(evSetBG, "row.names") #get row.names as integers
		  trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
		  # set samples to only those groups that remain
		  ssVec <- sampSizeVec[!names(sampSizeVec) == group$vals[[i]]]
		  #ssVec["pseu-a"] <- sum(ssVec[!names(ssVec) %in% "pseu-a"])
		  
		  # join em, clean up
		  trSet <- rbind(trSet, trSetBG)
		  trSet[,group$colNm] <- factor(trSet[,group$colNm])
		  evSet[[i]] <- rbind(evSet[[i]], evSetBG)
		  
		  
		  rm(trSelStr, evSelStr, trSetBG, evSetBG, TrBGsamps, BGsampSz )
		  
		  trRes[[i]] <- randomForest(trSet[,indVarCols],y=trSet[,depVarCol],
		                             importance=TRUE,ntree=ntrees,mtry=mtry,
		                             strata = trSet[,group$colNm], replace = TRUE, sampsize = ssVec
		                             )
		  
		  # run a randomForest predict on the validation data
		  evRes[[i]] <- predict(trRes[[i]], evSet[[i]], type="prob")
		   # use ROCR to structure the data. Get pres col of evRes (= named "1")
		  v.rocr.pred[[i]] <- prediction(evRes[[i]][,"1"],evSet[[i]]$pres) #
		   # extract the auc for metadata reporting
		  v.rocr.auc[[i]] <- performance(v.rocr.pred[[i]], "auc")@y.values[[1]]
			cat("finished run", i, "of", length(group$vals), "\n")
	}

	# restructure validation predictions so ROCR will average the figure
	v.rocr.pred.restruct <- v.rocr.pred[[1]]
	#send in the rest
	for(i in 2:length(v.rocr.pred)){
		v.rocr.pred.restruct@predictions[[i]] <- v.rocr.pred[[i]]@predictions[[1]]
		v.rocr.pred.restruct@labels[[i]] <- v.rocr.pred[[i]]@labels[[1]]
		v.rocr.pred.restruct@cutoffs[[i]] <- v.rocr.pred[[i]]@cutoffs[[1]]
		v.rocr.pred.restruct@fp[[i]] <- v.rocr.pred[[i]]@fp[[1]]
		v.rocr.pred.restruct@tp[[i]] <- v.rocr.pred[[i]]@tp[[1]]
		v.rocr.pred.restruct@tn[[i]] <- v.rocr.pred[[i]]@tn[[1]]
		v.rocr.pred.restruct@fn[[i]] <- v.rocr.pred[[i]]@fn[[1]]
		v.rocr.pred.restruct@n.pos[[i]] <- v.rocr.pred[[i]]@n.pos[[1]]
		v.rocr.pred.restruct@n.neg[[i]] <- v.rocr.pred[[i]]@n.neg[[1]]
		v.rocr.pred.restruct@n.pos.pred[[i]] <- v.rocr.pred[[i]]@n.pos.pred[[1]]
		v.rocr.pred.restruct@n.neg.pred[[i]] <- v.rocr.pred[[i]]@n.neg.pred[[1]]
	}

	# run a ROC performance with ROCR
	v.rocr.rocplot.restruct <- performance(v.rocr.pred.restruct, "tpr","fpr")
	# send it to perf for the averaging lines that follow
	perf <- v.rocr.rocplot.restruct

	## for infinite cutoff, assign maximal finite cutoff + mean difference
	## between adjacent cutoff pairs  (this code is from ROCR)
	if (length(perf@alpha.values)!=0) perf@alpha.values <-
		lapply(perf@alpha.values,
			function(x) { isfin <- is.finite(x);
				x[is.infinite(x)] <-
					(max(x[isfin]) +
						mean(abs(x[isfin][-1] -
						x[isfin][-length(x[isfin])])));
				x[is.nan(x)] <- 0.001; #added by tgh to handle vectors length 2
		x})

	for (i in 1:length(perf@x.values)) {
		ind.bool <- (is.finite(perf@x.values[[i]]) & is.finite(perf@y.values[[i]]))
		if (length(perf@alpha.values) > 0)
			perf@alpha.values[[i]] <- perf@alpha.values[[i]][ind.bool]
		perf@x.values[[i]] <- perf@x.values[[i]][ind.bool]
		perf@y.values[[i]] <- perf@y.values[[i]][ind.bool]
	}
	perf.sampled <- perf

	# create a list of cutoffs to interpolate off of
	alpha.values <- rev(seq(min(unlist(perf@alpha.values)),
							max(unlist(perf@alpha.values)),
							length=max(sapply(perf@alpha.values, length))))
	# interpolate by cutoff, values for y and x
	for (i in 1:length(perf.sampled@y.values)) {
		perf.sampled@x.values[[i]] <-
		  approxfun(perf@alpha.values[[i]],perf@x.values[[i]],
					rule=2, ties=mean)(alpha.values)
		perf.sampled@y.values[[i]] <-
		  approxfun(perf@alpha.values[[i]], perf@y.values[[i]],
					rule=2, ties=mean)(alpha.values)
	}

	## compute average curve
	perf.avg <- perf.sampled
	perf.avg@x.values <- list(rowMeans( data.frame( perf.avg@x.values)))
	perf.avg@y.values <- list(rowMeans( data.frame( perf.avg@y.values)))
	perf.avg@alpha.values <- list( alpha.values )

	for(i in 1:length(group$vals)){
	  # get MTP: minimum training presence (minimum votes recieved [probability]
	  # for any training point)
	  allVotesPrespts <- trRes[[i]]$votes[,"1"][trRes[[i]]$y==1]
	  MTP <- min(allVotesPrespts, na.rm = TRUE)

	  if(MTP == 0) {
	    # max sensitivity plus specificity (maxSSS per Liu et al 2016)
	    # create the prediction object for ROCR. Get pres col from y, prediction from votes (=named "1")
	    pred <- prediction(trRes[[i]]$votes[,"1"],trRes[[i]]$y)
	    sens <- performance(pred,"sens")
	    spec <- performance(pred,"spec")
	    sss <- data.frame(cutSens = unlist(sens@x.values),sens = unlist(sens@y.values),
	                      cutSpec = unlist(spec@x.values), spec = unlist(spec@y.values))
	    sss$sss <- with(sss, sens + spec)
	    maxSSS <- sss[which.max(sss$sss),"cutSens"]#/numCores
	    cutval.rf <- c(1-maxSSS, maxSSS)
	    names(cutval.rf) <- c("0","1")
	  } else if(MTP == 1) {
	    cat("MTP == 1 in validation loop \n")
	    newCut <- 0.99
	    cutval.rf <- c(1-newCut, newCut)
	    names(cutval.rf) <- c("0","1")
    } else {
	    cutval.rf <- c(1-MTP, MTP)
	    names(cutval.rf) <- c("0","1")
	  }

		#apply the cutoff to the validation data
	  v.rf.pred.cut <- predict(trRes[[i]], evSet[[i]],type="response", cutoff=cutval.rf)
		#make the confusion matrix
		v.y[[i]] <- table(observed = evSet[[i]][,"pres"],
			predicted = v.rf.pred.cut)
		#add estimated accuracy measures
		v.y[[i]] <- cbind(v.y[[i]],
			"accuracy" = c(v.y[[i]][1,1]/sum(v.y[[i]][1,]), v.y[[i]][2,2]/sum(v.y[[i]][2,])))
		#add row, col names
		rownames(v.y[[i]])[rownames(v.y[[i]]) == "0"] <- "background/abs"
		rownames(v.y[[i]])[rownames(v.y[[i]]) == "1"] <- "known pres"
		colnames(v.y[[i]])[colnames(v.y[[i]]) == "0"] <- "pred. abs"
		colnames(v.y[[i]])[colnames(v.y[[i]]) == "1"] <- "pred. pres"
		print(v.y[[i]])
		#Generate kappa statistics for the confusion matrices
		v.kappa[[i]] <- Kappa(v.y[[i]][1:2,1:2])
		#True Skill Statistic
		v.tss[[i]] <- v.y[[i]][2,3] + v.y[[i]][1,3] - 1
		#Overall Accuracy
		v.OvAc[[i]] <- (v.y[[i]][[1,1]]+v.y[[i]][[2,2]])/sum(v.y[[i]][,1:2])
		### importance measures ###
		#count the number of variables
		n.var <- nrow(trRes[[i]]$importance)
		#get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
		imp <- importance(trRes[[i]], class = NULL, scale = TRUE, type = NULL)
		imp <- imp[,"MeanDecreaseAccuracy"]
		#get number of variables used in each forest
		used <- varUsed(trRes[[i]])
		names(used) <- names(imp)
		t.importance[[i]] <- data.frame("meanDecreaseAcc" = imp,
									"timesUsed" = used )
	} #close loop

	#housecleaning
	rm(trSet, evSet)

	#average relevant validation/summary stats
	# Kappa - weighted, then unweighted
	K.w <- unlist(v.kappa, recursive=TRUE)[grep("Weighted.value",
						names(unlist(v.kappa, recursive=TRUE)))]
	Kappa.w.summ <- data.frame("mean"=mean(K.w), "sd"=sd(K.w),"sem"= sd(K.w)/sqrt(length(K.w)))
	K.unw <- unlist(v.kappa, recursive=TRUE)[grep("Unweighted.value",
						names(unlist(v.kappa, recursive=TRUE)))]
	Kappa.unw.summ <- data.frame("mean"=mean(K.unw), "sd"=sd(K.unw),"sem"= sd(K.unw)/sqrt(length(K.unw)))
	#AUC - area under the curve
	auc <- unlist(v.rocr.auc)
	auc.summ <- data.frame("mean"=mean(auc), "sd"=sd(auc),"sem"= sd(auc)/sqrt(length(auc)))
	#TSS - True skill statistic
	tss <- unlist(v.tss) 
	tss.summ <- data.frame("mean"=mean(tss), "sd"=sd(tss),"sem"= sd(tss)/sqrt(length(tss)))
	#Overall Accuracy
	OvAc <- unlist(v.OvAc)
	OvAc.summ <- data.frame("mean"=mean(OvAc), "sd"=sd(OvAc),"sem"= sd(OvAc)/sqrt(length(OvAc)))
	#Specificity and Sensitivity
	v.y.flat <- abind(v.y,along=1)  #collapsed confusion matrices
	v.y.flat.sp <- v.y.flat[rownames(v.y.flat)=="background/abs",]
	v.y.flat.sp <- as.data.frame(v.y.flat.sp, row.names = 1:length(v.y.flat.sp[,1]))
	specif <- v.y.flat.sp[,"pred. abs"]/(v.y.flat.sp[,"pred. abs"] + v.y.flat.sp[,"pred. pres"])   #specificity
	specif.summ <- data.frame("mean"=mean(specif), "sd"=sd(specif),"sem"= sd(specif)/sqrt(length(specif)))
	v.y.flat.sn <- v.y.flat[rownames(v.y.flat)=="known pres",]
	v.y.flat.sn <- as.data.frame(v.y.flat.sn, row.names = 1:length(v.y.flat.sn[,1]))
	sensit <- v.y.flat.sn[,"pred. pres"]/(v.y.flat.sn[,"pred. pres"] + v.y.flat.sn[,"pred. abs"])    #sensitivity
	sensit.summ <- data.frame("mean"=mean(sensit), "sd"=sd(sensit),"sem"= sd(sensit)/sqrt(length(sensit)))

	summ.table <- data.frame(Name=c("Weighted Kappa", "Unweighted Kappa", "AUC",
									"TSS", "Overall Accuracy", "Specificity",
									"Sensitivity"),
							 Mean=c(Kappa.w.summ$mean, Kappa.unw.summ$mean,auc.summ$mean,
									tss.summ$mean, OvAc.summ$mean, specif.summ$mean,
									sensit.summ$mean),
							 SD=c(Kappa.w.summ$sd, Kappa.unw.summ$sd,auc.summ$sd,
									tss.summ$sd, OvAc.summ$sd, specif.summ$sd,
									sensit.summ$sd),
							 SEM=c(Kappa.w.summ$sem, Kappa.unw.summ$sem,auc.summ$sem,
									tss.summ$sem, OvAc.summ$sem, specif.summ$sem,
									sensit.summ$sem))
	print(summ.table)
} else {
	cat("Less than 3 stratum, can't do validation", "\n")
	cutval <- NA
}

# increase the number of trees for the full model
ntrees <- 2000

####
#   run the full model ----
####

rf.full <- randomForest(df.full[,indVarCols],
                        y=df.full[,depVarCol],
                        importance=TRUE,
                        ntree=ntrees,
                        mtry=mtry,
                        strata = df.full[,group$colNm],
                        sampsize = sampSizeVec, replace = TRUE,
                        norm.votes = TRUE)
####
# Importance measures ----
####
# get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
f.imp <- importance(rf.full, class = NULL, scale = TRUE, type = NULL)
f.imp <- f.imp[,"MeanDecreaseAccuracy"]

db <- dbConnect(SQLite(),dbname=nm_db_file)  
# get importance data, set up a data frame
EnvVars <- data.frame(gridName = names(f.imp), impVal = f.imp, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVarsAqua WHERE gridName COLLATE NOCASE in ('", paste(EnvVars$gridName,sep=", "),
					"'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(EnvVars$gridName)) {
  try(EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}
##clean up
dbDisconnect(db)

###
# partial plot data ----
###
#get the order for the importance charts
ord <- order(EnvVars$impVal, decreasing = TRUE)[1:length(indVarCols)]
#set up a list to hold the plot data
n.plots <- min(c(length(f.imp), 9))
pPlots <- vector("list",n.plots)
		names(pPlots) <- c(1:n.plots)
#get the top partial plots
pplotSamp <- min(c(length(df.full[,1])/10, 10000)) # take 10% of samples, or 10000, whichever is less
pplotSamp <- sample(1:length(df.full[,1]), size = round(pplotSamp), replace = F)
for(i in 1:n.plots){
  curvar <- names(f.imp[ord[i]])
  pPlots[[i]] <- do.call("partialPlot", list(x = rf.full, pred.data = df.full[pplotSamp,indVarCols],
                                             x.var = curvar,
                                             which.class = 1,
                                             plot = FALSE))
  pPlots[[i]]$gridName <- curvar
  pPlots[[i]]$fname <- EnvVars$fullName[ord[i]]
  cat("finished partial plot ", i, " of ",n.plots, "\n")
}
rm(curvar)

# save the project, return to the original working directory
dir.create(paste0(loc_model, "/", model_species,"/outputs/rdata"), recursive = T, showWarnings = F)
setwd(paste0(loc_model, "/", model_species,"/outputs"))

for(i in 1:length(modelrun_meta_data)) assign(names(modelrun_meta_data)[i], modelrun_meta_data[[i]])
ls.save <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("begin_step","rdata","prompt","scrpt",
                                                             "run_steps","prompt","fn_args", names(fn_args))]
save(list = ls.save, file = paste0("rdata/", model_run_name,".Rdata"), envir = environment())

# write model metadata to db
# tblModelResults
db <- dbConnect(SQLite(),dbname=nm_db_file)  
tblModelResults <- data.frame(model_run_name = model_run_name, EGT_ID = ElementNames$EGT_ID, table_code = baseName,
                              internal_comments = model_comments, metadata_comments = metaData_comments,
                              model_comp_name = model_comp_name, modeller = modeller,
                              model_start_time = model_start_time, model_end_time = as.character(Sys.time()),
                              r_version = r_version, repo_head = repo_head, seed = seed)
dbWriteTable(db, "tblModelResults", tblModelResults, append = T)

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, gridName = tolower(envvar_list), inFinalModel = 0)
varImpDB <- merge(varImpDB, EnvVars[c("gridName","impVal")], by = "gridName", all.x = T)
varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = T)
dbDisconnect(db)

message(paste0("Saved rdata file: '", model_run_name , "'."))
