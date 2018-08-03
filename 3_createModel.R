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

setwd(loc_modelIn)

#get a list of what's in the directory
p_fileList <- dir( pattern = "_att.csv$")
p_fileList
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1
presFile <- p_fileList[[n]]
fileElemCode <- gsub("_att.csv$", "", presFile)
# get the presence points
df.in <-read.csv(presFile, colClasses=c("huc12"="character"))

# absence points
bk_fileList <- dir( pattern = "_clean.csv$")
bk_fileList
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1
bkgFile <- bk_fileList[[n]]
df.abs <- read.csv(bkgFile, colClasses=c("huc12"="character"))
# get a list of env-vars for later checking of ev presence in the database
envvar_list <- names(df.abs)[!names(df.abs) %in% c("huc12","comid")] # gets a list of environmental variables

#make sure we don't have any NAs
df.in <- df.in[complete.cases(df.in[,!names(df.in) %in% c("obsdate","date")]),]  # to ensure missing dates are not excluding records

# align data sets, QC ----
# add some fields to each
df.in <- cbind(df.in, pres=1)
df.abs$stratum <- "pseu-a"
df.abs <- cbind(df.abs, EO_ID_ST="pseu-a", pres=0, SNAME="background")

# lower case column names
names(df.in) <- tolower(names(df.in))
names(df.abs) <- tolower(names(df.abs))

# are these all in the lookup database? Checking here.
db <- dbConnect(SQLite(),dbname=nm_db_file)  
op <- options("useFancyQuotes") 
options(useFancyQuotes = FALSE) #sQuote call unhappy with fancy quote, turn off
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVarsAqua WHERE gridName in (", 
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

# trust that the desired env vars are in df.in
envvar_list <- envvar_list[envvar_list %in% names(df.in)]

#clean up
options(op)
dbDisconnect(db)
rm(db)

# clean up, merge data sets -----
df.in$x.1 <- NULL # can probably eliminate with better csv data prep from GIS
df.in$x <- NULL # can probably eliminate with better csv data prep from GIS
df.in$scomname <- NULL  # not in df.abs --> causing issues on the rearrange below
df.abs$x <- NULL # can probably eliminate with better csv data prep from GIS
df.abs$x.1 <- NULL # can probably eliminate with better csv data prep from GIS

# add a 'stratum' column to df.in for jackknife procedure [MAKE SURE TO ASSIGN DESIRED COLUMN HERE]
df.in$stratum <- as.character(df.in$group_id) # group_id used for model stratification

# this is the full list of fields, arranged appropriately
colList <- c("sname","eo_id_st","pres","stratum","comid", "huc12", envvar_list)
#colList <- names(df.in)

# if colList gets modified, 
# also modify the locations for the independent and dependent variables, here
depVarCol <- 3 # 'pres'
indVarCols <- c(7:length(colList)) 

#re-arrange
df.in <- df.in[,colList]
df.abs <- df.abs[,colList]
# now remove absence rows with NAs
df.abs <- df.abs[complete.cases(df.abs),]

#Fire up SQLite
db <- dbConnect(SQLite(),dbname=nm_db_file)  
  
ElementNames <- as.list(c(SciName=as.character(df.in[1,"sname"]), CommName="", Code=fileElemCode, Type=""))

# populate the common name, elemtype field
SQLquery <- paste("SELECT COMMONNAME, ELEMTYPE FROM lkpSpecies WHERE CODE = '", 
                  ElementNames["Code"],"';", sep="")
ElementNames[c(2,4)] <- dbGetQuery(db, statement = SQLquery)[1,]

#also get correlated env var information
SQLquery <- "SELECT gridName, correlatedVarGroupings FROM lkpEnvVarsAqua WHERE correlatedVarGroupings IS NOT NULL;"
corrdEVs <- dbGetQuery(db, statement = SQLquery)
corrdEVs <- corrdEVs[corrdEVs$gridName %in% envvar_list,]

dbDisconnect(db)
rm(db)

# row bind the pseudo-absences with the presence reaches
df.abs$eo_id_st <- factor(df.abs$eo_id_st)
df.full <- rbind(df.in, df.abs)

# reset these factors
df.full$stratum <- factor(df.full$stratum) # this is set below, just resetting and maintaining the column order
df.full$eo_id_st <- factor(df.full$eo_id_st)
df.full$pres <- factor(df.full$pres)
df.full$huc12 <- factor(tolower(as.character(df.full$huc12)))
df.full$sname <- factor(df.full$sname)

# make sampSizeVec using assigned stratum
#sampSizeVec <- table(df.full$stratum) # CHANGE THIS?? (sample sizes by HUC12? would need to change pseu-abs record values in that case)
#sampSizeVec["pseu-a"] <- sum(sampSizeVec) - sampSizeVec["pseu-a"]  # set samples of absences equal to total presences

# make sampSizeVec using 75% of presences, same number of PAs
npres <- floor(sum(df.full$pres==1) * 0.75)
sampSizeVec <- c(npres, npres)
names(sampSizeVec) <- c("0", "1")
rm(npres)

##
# tune mtry ----
# run through mtry twice
x <- tuneRF(df.full[,indVarCols],
             y=df.full[,depVarCol],
             ntreeTry = 300, stepFactor = 2, mtryStart = 6,
            strata = df.full[,depVarCol], replace = TRUE, sampsize = sampSizeVec)

newTry <- x[x[,2] == min(x[,2]),1]

y <- tuneRF(df.full[,indVarCols],
            y=df.full[,depVarCol],
            ntreeTry = 300, stepFactor = 1.5, mtryStart = max(newTry),
            strata = df.full[,depVarCol], replace = TRUE, sampsize = sampSizeVec)

mtry <- max(y[y[,2] == min(y[,2]),1])
rm(x,y)

##
# Remove the least important env vars ----
##

ntrees <- 1000
rf.find.envars <- randomForest(df.full[,indVarCols],
                        y=df.full[,depVarCol],
                        importance=TRUE,
                        ntree=ntrees,
                        mtry=mtry,
                        strata = df.full[,depVarCol], replace = TRUE, sampsize = sampSizeVec) 

impvals <- importance(rf.find.envars, type = 1)
OriginalNumberOfEnvars <- length(impvals)

# first remove the bottom of the correlated vars
for(grp in unique(corrdEVs$correlatedVarGroupings)){
 vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
 imp.sub <- impvals[rownames(impvals) %in% vars,, drop = FALSE]
 varsToDrop <- imp.sub[!imp.sub == max(imp.sub),, drop = FALSE]
 impvals <- impvals[!rownames(impvals) %in% rownames(varsToDrop),,drop = FALSE]
}
rm(vars, imp.sub, varsToDrop)

# remove variables with negative/0 importance values (replaces percentile variable choosing, commented out below)
impEnvVars <- impvals[impvals > 0,]
# set the percentile, here choosing above 25% percentile
# envarPctile <- 0.5
# y <- quantile(impvals, probs = envarPctile)
# impEnvVars <- impvals[impvals > y,]
subsetNumberofEnvars <- length(impEnvVars)
# rm(y)

# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% names(impEnvVars)
impEnvVarCols[1:6] <- TRUE  # first 6 columns are fixed attributes, not env. vars
# subset!
df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(7:length(names(df.full))) # get index of env. var. columns (columns 7+)

##
# code above is for removing least important env vars
##

# prep for validation loop ----
#now that entire set is cleaned up, split back out to use any of the three DFs below
df.in2 <- subset(df.full,pres == "1")
df.abs2 <- subset(df.full, pres == "0")
df.in2$stratum <- factor(df.in2$stratum)
df.abs2$stratum <- factor(df.abs2$stratum)
df.in2$huc12 <- factor(df.in2$huc12) 
df.abs2$huc12 <- factor(df.abs2$huc12)
df.in2$pres <- factor(df.in2$pres)
df.abs2$pres <- factor(df.abs2$pres)

#reset the row names, needed for random subsetting method of df.abs2, below
row.names(df.in2) <- 1:nrow(df.in2)
row.names(df.abs2) <- 1:nrow(df.abs2)

#initialize the grouping list, and set up grouping variables
group <- vector("list")
group$colNm <- "stratum"
group$JackknType <- "adjacent presence reach groups" # CHANGE THIS IF STRATUM CHANGES
group$vals <- unique(df.in2$stratum)

#reduce the number of trees if group$vals has more than 30 entries (removed from Aquatic for now; fixed to 1000)
#this is for validation
#if(length(group$vals) > 30) {
#	ntrees <- 750
#} else {
#	ntrees <- 1000
#}

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
      
if(length(group$vals)>2){
	for(i in 1:length(group$vals)){
		   # Create an object that stores the select command, to be used by subset.
		  trSelStr <- parse(text=paste(group$colNm[1]," != '", group$vals[[i]],"'",sep=""))
		  evSelStr <- parse(text=paste(group$colNm[1]," == '", group$vals[[i]],"'",sep=""))
		   # apply the subset. do.call is needed so selStr can be evaluated correctly
		  trSet <- do.call("subset",list(df.in2, trSelStr))
		  evSet[[i]] <- do.call("subset",list(df.in2, evSelStr))
		   # use sample to grab a random subset from the background points
		  BGsampSz <- nrow(evSet[[i]]) * 10
		  evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz , replace = FALSE, prob = NULL),]
		   # get the other portion for the training set
		  TrBGsamps <- attr(evSetBG, "row.names") #get row.names as integers
		  trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
		   # join em, clean up
		  trSet <- rbind(trSet, trSetBG)
		  trSet$stratum <- factor(trSet$stratum)
		  evSet[[i]] <- rbind(evSet[[i]], evSetBG)
		  
		  # pseu-a is resized to match training sample (originally was not)
		  #ssVec <- sampSizeVec[!names(sampSizeVec) == group$vals[[i]]]
		  #ssVec["pseu-a"] <- sum(ssVec) - ssVec["pseu-a"]
		  #rm(trSetBG, evSetBG)
		  
		  # make sampSizeVec using 75% of presences, and same number of absences
		  npres <- floor(sum(trSet$pres==1) * 0.75)
		  ssVec <- c(npres, npres)
		  names(ssVec) <- c("0", "1")
		  rm(npres)
		  
		  trRes[[i]] <- randomForest(trSet[,indVarCols],y=trSet[,depVarCol],
		                             importance=TRUE,ntree=ntrees,mtry=mtry,
		                             # strata = trSet[,group$colNm], replace = TRUE, sampsize = ssVec
		                             strata = trSet[,depVarCol], replace = TRUE, sampsize = ssVec
		                             )
		  
		  # run a randomForest predict on the validation data
		  evRes[[i]] <- predict(trRes[[i]], evSet[[i]], type="prob")
		   # use ROCR to structure the data. Get pres col of evRes (= named "1")
		  v.rocr.pred[[i]] <- prediction(evRes[[i]][,"1"],evSet[[i]]$pres)
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

	# find the best cutoff based on the averaged ROC curve
	### TODO: customize/calculate this for each model rather than
	### average? 
	cutpt <- which.max(abs(perf.avg@x.values[[1]]-perf.avg@y.values[[1]]))
	cutval <- perf.avg@alpha.values[[1]][cutpt]
	cutX <- perf.avg@x.values[[1]][cutpt]
	cutY <- perf.avg@y.values[[1]][cutpt]
	cutval.rf <- c(1-cutval,cutval)
	names(cutval.rf) <- c("0","1")

	for(i in 1:length(group$vals)){
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
	summ.table
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
                        strata = df.full[,depVarCol],
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
for(i in 1:length(EnvVars$gridName)){
  EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2])
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
rm(curvar, n.plots, pplotSamp)

# save the project, return to the original working directory
setwd(loc_modelOut)
# set model_run_name
model_run_name <- paste0(ElementNames$Code, "_",
                         gsub(" ","_",gsub(c("-|:"),"",as.character(model_start_time))))
modelrun_meta_data$model_run_name <- model_run_name
# remove fn args/vars from the save object
ls.save <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("begin_step","rdata","prompt","scrpt",
                                                             "run_steps","prompt","fn_args", names(fn_args))]
save(list = ls.save, file = paste0("rdata/", model_run_name,".Rdata"), envir = environment())

# write model metadata to db
db <- dbConnect(SQLite(),dbname=nm_db_file)  
insert_values <- paste(model_run_name, ElementNames$Code, model_start_time, modeller, model_comp_name, r_version, model_comments, sep = "','")
SQLquery <- paste0("INSERT INTO tblModelRuns (modelRunName, CODE, modelBeginTime, modeller, modelCompName, rVersion, internalComments)
  VALUES ('",insert_values,"');")
dbExecute(db, SQLquery)

message(paste0("Saved rdata file: '", model_run_name , "'."))
