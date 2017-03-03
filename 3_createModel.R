# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points
# - build partial plots of top performing env vars for metadata output

# assumptions:
# input table contains only one species

library(RSQLite)
library(ROCR)    #for ROC plots and stats
library(vcd)     #for kappa stats
library(abind)   #for collapsing the nested lists
library(foreign) #for reading dbf files
library(randomForest)

#####
#  Lines that require editing
#
# set up paths ----
sppPtLoc <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/point_data"
ranPtLoc <- "K:/Reg5Modeling_Project/inputs/background"
dbLoc <- "K:/Reg5Modeling_Project/databases"
pathToRas <- "K:/Reg5Modeling_Project/inputs/env_vars/nativeR"

setwd(sppPtLoc)

# directory for saving RData files (analysis data)
rdataOut <- "K:/Reg5Modeling_Project/outputs"

# the names of the files to be uploaded: presence points
df.in <-read.dbf("glypmuhl_att.dbf")

# absence points
df.abs <- read.dbf(paste(ranPtLoc,"sdmclpbnd_20160831_buffNeg1000_att.dbf", sep="/"))

#  End, lines that require editing
#
#####

# align data sets, QC ----
# add some fields to each
df.in <- cbind(df.in, pres=1)
df.abs <- cbind(df.abs, EO_ID_ST=99999, 
					pres=0, RA="High", SNAME="background")

df.abs$stratum <- "pseu-a"

# lower case column names
names(df.in) <- tolower(names(df.in))
names(df.abs) <- tolower(names(df.abs))

# get a list of env vars from the folder used to create the raster stack
raslist <- list.files(path = pathToRas, pattern = ".grd$")
rasnames <- gsub(".grd", "", raslist)

# are these all in the lookup database? Checking here.
db_file <- paste(dbLoc, "SDM_lookupAndTracking.sqlite", sep = "/")
db <- dbConnect(SQLite(),dbname=db_file)  
op <- options("useFancyQuotes") 
options(useFancyQuotes = FALSE) #sQuote call unhappy with fancy quote, turn off
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName in (", 
                  toString(sQuote(rasnames)),
                  "); ", sep = "")
namesInDB <- dbGetQuery(db, statement = SQLquery)
namesInDB$gridName <- tolower(namesInDB$gridName)
rasnames <- tolower(rasnames)

## this prints rasters not in the lookup database
## if blank you are good to go, otherwise figure out what's up
rasnames[!rasnames %in% namesInDB$gridName]

## this prints out the rasters that don't appear as a column name
## in df.in (meaning it wasn't used to attribute or the name is funky)
## if blank you are good to go
rasnames[!rasnames %in% names(df.in)]

# clean up
options(op)
dbDisconnect(db)
rm(db)

# this is the full list of fields, arranged appropriately
colList <- c("sname","eo_id_st","pres","stratum", "ra", rasnames)

# if colList gets modified, 
# also modify the locations for the independent and dependent variables, here
depVarCol <- 3
indVarCols <- c(6:length(colList))

#re-arrange
df.in <- df.in[,colList]
df.abs <- df.abs[,colList]

#remove nulls
df.abs<-df.abs[complete.cases(df.abs),]
df.in<-df.in[complete.cases(df.in),]

#Fire up SQLite
db <- dbConnect(SQLite(),dbname=db_file)  
  
ElementNames <- as.list(c(SciName="",CommName="",Code="",Type=""))
ElementNames[1] <- as.character(df.in[1,"sname"])

# get the names used in metadata output
SQLquery <- paste("SELECT CODE FROM lkpSpecies WHERE SCIEN_NAME = '", 
	ElementNames[1],"' ;", sep="")
ElementNames[3] <- as.list(dbGetQuery(db, statement = SQLquery)[1,1])
# populate the common name field
SQLquery <- paste("SELECT COMMONNAME FROM lkpSpecies WHERE SCIEN_NAME = '", 
	ElementNames[1],"';", sep="")
ElementNames[2] <- dbGetQuery(db, statement = SQLquery)
# populate element type (A or P)
SQLquery <- paste("SELECT ELEMTYPE FROM lkpSpecies WHERE SCIEN_NAME = '", 
	ElementNames[1],"';", sep="")
ElementNames[4] <- as.list(dbGetQuery(db, statement = SQLquery)[1,1])
ElementNames
dbDisconnect(db)
rm(db)

# row bind the pseudo-absences with the presence points
df.abs$eo_id_st <- factor(df.abs$eo_id_st)
df.full <- rbind(df.in, df.abs)

# reset these factors
df.full$stratum <- factor(df.full$stratum)
df.full$eo_id_st <- factor(df.full$eo_id_st)
df.full$pres <- factor(df.full$pres)
df.full$ra <- factor(df.full$ra)
df.full$sname <- factor(df.full$sname)
	
##
# tune mtry ----
# run through mtry twice

x <- tuneRF(df.full[,indVarCols],
             y=df.full[,depVarCol],
             ntreeTry = 50, stepFactor = 2, mtryStart = 6)

newTry <- x[x[,2] == min(x[,2]),1]

y <- tuneRF(df.full[,indVarCols],
            y=df.full[,depVarCol],
            ntreeTry = 50, stepFactor = 1.5, mtryStart = max(newTry))

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
                        mtry=mtry)

impvals <- importance(rf.find.envars, type = 1)
OriginalNumberOfEnvars <- length(impvals)
# set the percentile, here choosing above 25% percentile
envarPctile <- 0.90
y <- quantile(impvals, probs = envarPctile)
impEnvVars <- impvals[impvals > y,]
subsetNumberofEnvars <- length(impEnvVars)
rm(y)
# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% names(impEnvVars)
impEnvVarCols[1:5] <- TRUE
# subset!
df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(6:length(names(df.full)))


### try out VSURF
library(VSURF)
vsurf.out <- VSURF(df.full[,indVarCols],
                   y=df.full[,depVarCol], parallel = TRUE)


### try out Hapfelmeier code ## Killed it, was still running after winter break!
# library(party)
# source("K:/Reg5Modeling_Project/scripts/Hapfelmeier_Appendices/Variable_Selektion_Approaches.r")
# 
# rfNAP.out <- NAP(X=df.full[,indVarCols],
#                  Y=df.full[,depVarCol])

##
# code above is for removing least important env vars
##

# prep for validation loop ----
#now that entire set is cleaned up, split back out to use any of the three DFs below
df.in2 <- subset(df.full,pres == "1")
df.abs2 <- subset(df.full, pres == "0")
df.in2$stratum <- factor(df.in2$stratum)
df.abs2$stratum <- factor(df.abs2$stratum)
df.in2$eo_id_st <- factor(df.in2$eo_id_st)
df.abs2$eo_id_st <- factor(df.abs2$eo_id_st)
df.in2$pres <- factor(df.in2$pres)
df.abs2$pres <- factor(df.abs2$pres)

#reset the row names, needed for random subsetting method of df.abs2, below
row.names(df.in2) <- 1:nrow(df.in2)
row.names(df.abs2) <- 1:nrow(df.abs2)


#how many polygons do we have?
numPys <-  nrow(table(df.in2$stratum))
#how many EOs do we have?
numEOs <- nrow(table(df.in2$eo_id_st))

#initialize the grouping list, and set up grouping variables
#if we have fewer than 10 EOs, move forward with jackknifing by polygon, otherwise
#jackknife by EO.
group <- vector("list")
group$colNm <- ifelse(numEOs < 10,"stratum","eo_id_st")
group$JackknType <- ifelse(numEOs < 10,"polygon","element occurrence")
if(numEOs < 10) {
		group$vals <- unique(df.in2$stratum)
} else {
		group$vals <- unique(df.in2$eo_id_st)
}

# # reduce the number of groups, if there are more than 200, to 200 groups
# # this groups the groups simply if they are adjacent in the order created above.
if(length(group$vals) > 200) {
  in.cut <- cut(1:length(group$vals), b = 200)
  in.split <- split(group$vals, in.cut)
  names(in.split) <- NULL
  group$vals <- in.split
  group$JackknType <- paste(group$JackknType, ", grouped to 200 levels,", sep = "")
}

#reduce the number of trees if group$vals has more than 30 entries
#this is for validation
if(length(group$vals) > 30) {
	ntrees <- 500
} else {
	ntrees <- 750
}
###### reduced for testing #####
### TODO: clear when running real models
#ntrees <- 50

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
		  BGsampSz <- nrow(evSet[[i]])
		  evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz , replace = FALSE, prob = NULL),]
		   # get the other portion for the training set
		  TrBGsamps <- attr(evSetBG, "row.names") #get row.names as integers
		  trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
		   # join em, clean up
		  trSet <- rbind(trSet, trSetBG)
		  evSet[[i]] <- rbind(evSet[[i]], evSetBG)
		  rm(trSetBG, evSetBG)
		   # run RF on subsets
		  trRes[[i]] <- randomForest(trSet[,indVarCols],y=trSet[,depVarCol],
									 importance=TRUE,ntree=ntrees,mtry=mtry)
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
		rownames(v.y[[i]]) <- c("background/abs", "known pres")
		colnames(v.y[[i]]) <- c("pred. abs", "pred. pres", "accuracy")
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
		#housecleaning to save memory
		trRes[[i]]$forest <- NULL
		trRes[[i]]$oob.times <- NULL
		trRes[[i]]$votes <- NULL
		trRes[[i]]$predicted <- NULL
	} #close loop

	#housecleaning
	rm(trSet, evSet)

	#average relevant validation/summary stats
	# Kappa - wieghted, then unweighted
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
	specif <- v.y.flat.sp[,1]/(v.y.flat.sp[,1] + v.y.flat.sp[,2])   #specificity
	specif.summ <- data.frame("mean"=mean(specif), "sd"=sd(specif),"sem"= sd(specif)/sqrt(length(specif)))
	v.y.flat.sn <- v.y.flat[rownames(v.y.flat)=="known pres",]
	sensit <- v.y.flat.sn[,2]/(v.y.flat.sn[,1] + v.y.flat.sn[,2])    #sensitivity
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
	cat("Only one polygon, can't do validation", "\n")
	cutval <- NA
}

# increase the number of trees for the full model
ntrees <- 2000
   
####
#   run the full model ----
####

cat("running full model", "\n")
rf.full <- randomForest(df.full[,indVarCols],
						y=df.full[,depVarCol],
						importance=TRUE,
						ntree=ntrees,
						mtry=mtry, 
						sampsize = c(numEOs*10, numEOs),
						norm.votes = TRUE)


####
# Importance measures ----
####
#get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
f.imp <- importance(rf.full, class = NULL, scale = TRUE, type = NULL)
f.imp <- f.imp[,"MeanDecreaseAccuracy"]

db <- dbConnect(SQLite(),dbname=db_file)  
# get importance data, set up a data frame
EnvVars <- data.frame(gridName = names(f.imp), impVal = f.imp, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName in ('", paste(EnvVars$gridName,sep=", "),
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
ord <- order(EnvVars$impVal, decreasing = TRUE)[1:n.var]
#set up a list to hold the plot data
pPlots <- vector("list",8)
		names(pPlots) <- c(1:8)
#get the top eight partial plots
for(i in 1:8){
	pPlots[[i]] <- partialPlot(rf.full, df.full[,indVarCols],
						names(f.imp[ord[i]]),
						which.class = 1,
						plot = FALSE)
	pPlots[[i]]$gridName <- names(f.imp[ord[i]])
	pPlots[[i]]$fname <- EnvVars$fullName[ord[i]]
	cat("finished partial plot ", i, " of 8", "\n")
	}

#save the project, return to the original working directory
setwd(rdataOut)
save.image(file = paste(ElementNames$Code, ".Rdata", sep=""))
setwd(sppPtLoc)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())



## variable selection



