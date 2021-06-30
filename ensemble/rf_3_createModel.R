# File: 3_createModel.R
# Purpose: to create the random forest model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

library(randomForest)
library(iterators)
library(doParallel)



##
# tune mtry ----
#subset df to ten times pres for getting mtry  ## to increase speed
rowCounts <- table(df.full$pres)
if(rowCounts["0"] > (10 * rowCounts["1"])){
  tunePres <- df.full[df.full$pres == 1, ]
  tuneAbs <- df.full[df.full$pres == 0, ]
  tuneAbs <- tuneAbs[sample(nrow(tuneAbs), nrow(tunePres)* 10),]
  df.tune <- rbind(tunePres, tuneAbs)
  rm(tuneAbs, tunePres)
} else {
  df.tune <- df.full
}
# run through mtry twice
# very small numbers of polys can make a perfect prediction, especially
# when subsetted like above. If error (prediction error == 0) results from 
# subsetting, catch error and try tuning with full set
mtry <- tryCatch(
  {
  x <- tuneRF(df.tune[,indVarCols],
             y=df.tune[,depVarCol],
             ntreeTry = 300, stepFactor = 2, mtryStart = 6,
            strata = df.full[,group$colNm], sampsize = sampSizeVec, replace = TRUE)
  newTry <- x[x[,2] == min(x[,2]),1]
  y <- tuneRF(df.tune[,indVarCols],
              y=df.tune[,depVarCol],
              ntreeTry = 300, stepFactor = 1.5, mtryStart = max(newTry),
              strata = df.full[,group$colNm], sampsize = sampSizeVec, replace = TRUE)
  
  mtry <- max(y[y[,2] == min(y[,2]),1])
  rm(x,y, df.tune, newTry)
  mtry
  }, 
  error=function(cond) {
    message("Can't tune with subset, using full set.")
    df.tune <- df.full
    x <- tuneRF(df.tune[,indVarCols],
                y=df.tune[,depVarCol],
                ntreeTry = 300, stepFactor = 2, mtryStart = 6,
                strata = df.full[,group$colNm], sampsize = sampSizeVec, replace = TRUE)
    newTry <- x[x[,2] == min(x[,2]),1]
    y <- tuneRF(df.tune[,indVarCols],
                y=df.tune[,depVarCol],
                ntreeTry = 300, stepFactor = 1.5, mtryStart = max(newTry),
                strata = df.full[,group$colNm], sampsize = sampSizeVec, replace = TRUE)
    
    mtry <- max(y[y[,2] == min(y[,2]),1])
    rm(x,y, df.tune, newTry)
    mtry
  }
)
rm(rowCounts)

###
# Remove the least important env vars ----
##

ntrees <- 1000
numCores <- 10        

# do all randomForest calls in parallel, starting with this one
cl <- makeCluster(numCores)   
registerDoParallel(cl)

# pass libPath to workers
x <- clusterCall(cl, function(x) .libPaths(x), .libPaths())

treeSubs <- ceiling(ntrees/numCores)

rf.find.envars <- foreach(ntree = rep(treeSubs,numCores), .combine = randomForest::combine, 
                    .packages = 'randomForest', .multicombine = TRUE) %dopar% {
                     randomForest(df.full[,indVarCols],
                                  y=df.full[,depVarCol],
                                  importance=TRUE,
                                  ntree=ntree,
                                  mtry=mtry,
                                  strata = df.full[,group$colNm],
                                  sampsize = sampSizeVec, replace = TRUE,
                                  norm.votes = TRUE)
                   }


impvals <- importance(rf.find.envars, type = 1)
OriginalNumberOfEnvars <- length(impvals)

# first remove the bottom of the correlated vars
corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% row.names(impvals),]
if(nrow(corrdEVs) > 0 ){
  for(grp in unique(corrdEVs$correlatedVarGroupings)){
    vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
    imp.sub <- impvals[rownames(impvals) %in% vars,, drop = FALSE]
    suppressWarnings(varsToDrop <- imp.sub[!imp.sub == max(imp.sub),, drop = FALSE])
    impvals <- impvals[!rownames(impvals) %in% rownames(varsToDrop),,drop = FALSE]
  }
  rm(vars, imp.sub, varsToDrop)
}

# set the percentile, here choosing above 35% percentile
envarPctile <- 0.50
y <- quantile(impvals, probs = envarPctile)
impEnvVars <- impvals[impvals > y,]
subsetNumberofEnvars <- length(impEnvVars)
rm(y)
# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% names(impEnvVars)
impEnvVarCols[1:5] <- TRUE
# subset!
rf.df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(6:length(names(rf.df.full)))

rm(impvals, impEnvVars, impEnvVarCols)

##
# code above is for removing least important env vars
##

# prep for validation loop ----
#now that entire set is cleaned up, split back out to use any of the three DFs below
df.in2 <- subset(rf.df.full,pres == "1")
df.abs2 <- subset(rf.df.full, pres == "0")
df.in2$stratum <- factor(df.in2$stratum)
df.abs2$stratum <- factor(df.abs2$stratum)
df.in2$group_id <- factor(df.in2$group_id)
df.abs2$group_id <- factor(df.abs2$group_id)
df.in2$pres <- factor(df.in2$pres)
df.abs2$pres <- factor(df.abs2$pres)

#reset the row names, needed for random subsetting method of df.abs2, below
row.names(df.in2) <- 1:nrow(df.in2)
row.names(df.abs2) <- 1:nrow(df.abs2)

#reduce the number of trees if group$vals has more than 30 entries #commented out to be parallel with aquatic
#this is for validation
# if(length(group$vals) > 30) {
# 	ntrees <- 500
# } else {
# 	ntrees <- 750
# }
ntrees <- 1000

# reduce the number of validation loops if more than 50. 50 is plenty!
# randomly draw to get the validation set.
if(length(group$vals) > 50) {
  group$vals <- sample(group$vals, size = 50)
  group$vals <- factor(group$vals)
} 

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

# calculate the number of trees to send to each core
treeSubs <- ceiling(ntrees/numCores)

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
		  #### TEMPORARY DURING TESTING ####
      if(BGsampSz > nrow(df.abs2)) BGsampSz <- nrow(df.abs2)/2
		  evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz , replace = FALSE, prob = NULL),]
		   # get the other portion for the training set
		  TrBGsamps <- attr(evSetBG, "row.names") #get row.names as integers
		  trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
		   # join em, clean up
		  trSet <- rbind(trSet, trSetBG)
		  trSet[,group$colNm] <- factor(trSet[,group$colNm])
		  evSet[[i]] <- rbind(evSet[[i]], evSetBG)
		  
		  ssVec <- sampSizeVec[!names(sampSizeVec) == group$vals[[i]]]
		  # re-calc pseudo-absence samples to match input training samples if you can
		  tot_pseua <- ifelse(sum(ssVec[!names(ssVec) %in% "pseu-a"]) > nrow(trSetBG),
		                      nrow(trSetBG), 
		                      sum(ssVec[!names(ssVec) %in% "pseu-a"]))
		  ssVec["pseu-a"] <- tot_pseua
		  
		  rm(trSelStr, evSelStr, trSetBG, evSetBG, TrBGsamps, BGsampSz )

		  trRes[[i]] <- foreach(ntree = rep(treeSubs,numCores), .combine = randomForest::combine, 
		                        .packages = 'randomForest', .multicombine = TRUE) %dopar%
          		    		        randomForest(trSet[,indVarCols],y=trSet[,depVarCol],
          		                             importance=TRUE,mtry=mtry,ntree = ntree,
          		                             strata = trSet[,group$colNm], sampsize = ssVec, replace = TRUE
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
	rf.perf <- v.rocr.rocplot.restruct
  rm(v.rocr.rocplot.restruct)
	## for infinite cutoff, assign maximal finite cutoff + mean difference
	## between adjacent cutoff pairs  (this code is from ROCR)
	if (length(rf.perf@alpha.values)!=0) rf.perf@alpha.values <-
		lapply(rf.perf@alpha.values,
			function(x) { isfin <- is.finite(x);
				x[is.infinite(x)] <-
					(max(x[isfin]) +
						mean(abs(x[isfin][-1] -
						x[isfin][-length(x[isfin])])));
				x[is.nan(x)] <- 0.001; #added by tgh to handle vectors length 2
		x})

	for (i in 1:length(rf.perf@x.values)) {
		ind.bool <- (is.finite(rf.perf@x.values[[i]]) & is.finite(rf.perf@y.values[[i]]))
		if (length(rf.perf@alpha.values) > 0)
			rf.perf@alpha.values[[i]] <- rf.perf@alpha.values[[i]][ind.bool]
		rf.perf@x.values[[i]] <- rf.perf@x.values[[i]][ind.bool]
		rf.perf@y.values[[i]] <- rf.perf@y.values[[i]][ind.bool]
	}
	rf.perf.sampled <- rf.perf

	# create a list of cutoffs to interpolate off of
	alpha.values <- rev(seq(min(unlist(rf.perf@alpha.values)),
							max(unlist(rf.perf@alpha.values)),
							length=max(sapply(rf.perf@alpha.values, length))))
	# interpolate by cutoff, values for y and x
	for (i in 1:length(rf.perf.sampled@y.values)) {
		rf.perf.sampled@x.values[[i]] <-
		  approxfun(rf.perf@alpha.values[[i]],rf.perf@x.values[[i]],
					rule=2, ties=mean)(alpha.values)
		rf.perf.sampled@y.values[[i]] <-
		  approxfun(rf.perf@alpha.values[[i]], rf.perf@y.values[[i]],
					rule=2, ties=mean)(alpha.values)
	}

	## compute average curve
	rf.perf.avg <- rf.perf.sampled
	rf.perf.avg@x.values <- list(rowMeans( data.frame( rf.perf.avg@x.values)))
	rf.perf.avg@y.values <- list(rowMeans( data.frame( rf.perf.avg@y.values)))
	rf.perf.avg@alpha.values <- list( alpha.values )

	for(i in 1:length(group$vals)){
	  ### get threshold
	  # get MTP: minimum training presence (minimum votes recieved [probability]
	  # for any training point)
	  allVotesPrespts <- trRes[[i]]$votes[,"1"][trRes[[i]]$y == 1]
	  MTP <- min(allVotesPrespts/numCores)
    # calculations fail if MTP = 0 so if it does, fall back to maxSSS
	  if(MTP == 0) {
	    # max sensitivity plus specificity (maxSSS per Liu et al 2016)
	    # create the prediction object for ROCR. Get pres col from y, prediction from votes (=named "1")
	    pred <- prediction(trRes[[i]]$votes[,"1"],trRes[[i]]$y)
	    sens <- performance(pred,"sens")
	    spec <- performance(pred,"spec")
	    sss <- data.frame(cutSens = unlist(sens@x.values),sens = unlist(sens@y.values),
	                      cutSpec = unlist(spec@x.values), spec = unlist(spec@y.values))
	    sss$sss <- with(sss, sens + spec)
	    maxSSS <- sss[which.max(sss$sss),"cutSens"]/numCores
	    cutval.rf <- c(1-maxSSS, maxSSS)
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
	v.y.flat.sp <- as.data.frame(v.y.flat.sp, row.names = 1:length(v.y.flat.sp[,1]))
	specif <- v.y.flat.sp[,"pred. abs"]/(v.y.flat.sp[,"pred. abs"] + v.y.flat.sp[,"pred. pres"])   #specificity
	specif.summ <- data.frame("mean"=mean(specif), "sd"=sd(specif),"sem"= sd(specif)/sqrt(length(specif)))
	v.y.flat.sn <- v.y.flat[rownames(v.y.flat)=="known pres",]
	v.y.flat.sn <- as.data.frame(v.y.flat.sn, row.names = 1:length(v.y.flat.sn[,1]))
	sensit <- v.y.flat.sn[,"pred. pres"]/(v.y.flat.sn[,"pred. pres"] + v.y.flat.sn[,"pred. abs"])    #sensitivity
	sensit.summ <- data.frame("mean"=mean(sensit), "sd"=sd(sensit),"sem"= sd(sensit)/sqrt(length(sensit)))

	summ.table <- data.frame(metric=c("Weighted Kappa", "Unweighted Kappa", "AUC",
									"TSS", "Overall Accuracy", "Specificity",
									"Sensitivity"),
							metric_mn=c(Kappa.w.summ$mean, Kappa.unw.summ$mean,auc.summ$mean,
									tss.summ$mean, OvAc.summ$mean, specif.summ$mean,
									sensit.summ$mean),
							metric_sd=c(Kappa.w.summ$sd, Kappa.unw.summ$sd,auc.summ$sd,
									tss.summ$sd, OvAc.summ$sd, specif.summ$sd,
									sensit.summ$sd),
							metric_sem=c(Kappa.w.summ$sem, Kappa.unw.summ$sem,auc.summ$sem,
									tss.summ$sem, OvAc.summ$sem, specif.summ$sem,
									sensit.summ$sem))
	summ.table
} else {
	cat("Only one polygon, can't do validation", "\n")
	cutval <- NA
}

# increase the number of trees for the full model
ntrees <- 2000
treeSubs <- ceiling(ntrees/numCores)

####
#   run the full model ----
####
cat("... creating full model \n")

rf.full <- foreach(ntree = rep(treeSubs,numCores), .combine = randomForest::combine, 
                    .packages = 'randomForest', .multicombine = TRUE) %dopar% {
                        randomForest(rf.df.full[,indVarCols],
                              y=rf.df.full[,depVarCol],
                              importance=TRUE,
                              ntree=ntree,
                              mtry=mtry,
                              strata = rf.df.full[,group$colNm],
                              sampsize = sampSizeVec, replace = TRUE,
                              norm.votes = TRUE)
                              }

# write out input data
# connect to DB ..
db <- dbConnect(SQLite(),dbname=nm_db_file)

# write model input data to database before any other changes made
tblModelInputs <- data.frame(table_code = baseName,
                             model_run_name = model_run_name,
                             algorithm = algo,
                             EGT_ID = ElementNames$EGT_ID, 
                             datetime = as.character(Sys.time()),
                             feat_count = length(unique(df.in$stratum)), 
                             feat_grp_count = length(unique(df.in$group_id)),
                             jckn_grp_column = group$colNm,
                             jckn_grp_type = group$JackknType,
                             mn_grp_subsamp = mean(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             min_grp_subsamp = min(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             max_grp_subsamp = max(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             tot_obs_subsamp = sum(sampSizeVec[!names(sampSizeVec) == "pseu-a"]),
                             tot_bkgd_subsamp = sum(sampSizeVec[names(sampSizeVec) == "pseu-a"]),
                             obs_count = nrow(df.in), 
                             bkgd_count = nrow(df.abs)
)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append = TRUE)

# write validation data
summ.table <- cbind("model_run_name" = rep(model_run_name, nrow(summ.table)), 
                       "algorithm" = rep(algo, nrow(summ.table)), 
                    summ.table)

dbExecute(db, paste0("DELETE FROM tblModelResultsValidationStats where model_run_name = '", model_run_name, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelResultsValidationStats", summ.table, append = TRUE)


dbDisconnect(db)
rm(db)



####
# Importance measures ----
####
#get the importance measures (don't get GINI coeff - see Strobl et al. 2006)
f.imp <- importance(rf.full, class = NULL, scale = TRUE, type = NULL)
f.imp <- f.imp[,"MeanDecreaseAccuracy"]

db <- dbConnect(SQLite(),dbname=nm_db_file)  
# get importance data, set up a data frame
EnvVars <- data.frame(gridName = names(f.imp), impVal = f.imp, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", paste(EnvVars$gridName,sep=", "),
					"'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(EnvVars$gridName)) {
  try(EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}

###
# partial plot data ----
###
#get the order for the importance charts
ord <- order(EnvVars$impVal, decreasing = TRUE)[1:length(indVarCols)]
if(length(ord) > 15){
  pPlotListLen <- 15
} else {
  pPlotListLen <- length(ord)
}

cat("... calculating partial plots \n")

### subsample, grouped by pres/abs, to speed up partial plots
ppPres <- rf.df.full[rf.df.full$pres == 1, ]
ppAbs <- rf.df.full[rf.df.full$pres == 0, ]
ppPresSamp <- min(c(nrow(ppPres), 6000)) # take all pres samples, or 6000, whichever is less
ppPresSamp <- sample(1:nrow(ppPres), size = round(ppPresSamp), replace = FALSE)
ppPresSamp <- ppPres[ppPresSamp,]
ppAbsSamp <- min(c(nrow(ppAbs), 6000)) # take all abs samples, or 6000, whichever is less
ppAbsSamp <- sample(1:nrow(ppAbs), size = round(ppAbsSamp), replace = FALSE)
ppAbsSamp <- ppAbs[ppAbsSamp,]

ppPreddata <- rbind(ppPresSamp, ppAbsSamp)

# run partial plots in parallel
curvars = names(f.imp[ord])[1:pPlotListLen]
rf.pPlots <- foreach(i = iter(curvars), .packages = 'randomForest') %dopar% {
                  do.call("partialPlot", list(x = rf.full, pred.data = ppPreddata[,indVarCols],
                              x.var = i,
                              which.class = 1,
                              plot = FALSE))
}

#fill in names
names(rf.pPlots) <- c(1:pPlotListLen)
for(i in 1:length(rf.pPlots)){
  rf.pPlots[[i]]$gridName <- curvars[[i]]
  rf.pPlots[[i]]$fname <- EnvVars$fullName[ord[i]]
}
rm(ppPres, ppAbs, ppPresSamp, ppAbsSamp, ppPreddata)

# clear out registered clusters as caret is unhappy with them ...
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister()

stopCluster(cl)
rm(cl)

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, 
                       algorithm = algo, 
                       gridName = tolower(envvar_list), 
                       inFinalModel = 0)
varImpDB <- merge(varImpDB, EnvVars[c("gridName","impVal")], by = "gridName", all.x = T)
varImpDB$inFinalModel[!is.na(varImpDB$impVal)] <- 1
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = TRUE)
dbDisconnect(db)

closeAllConnections()
# 
# message(paste0("Saved rdata file: '", model_run_name , "'."))

### this isn't needed but keeping for a bit in one script in case we decide otherwise
#serious cleanup
# if(isNamespaceLoaded("doParallel")) unloadNamespace("doParallel")
# if(isNamespaceLoaded("foreach")) unloadNamespace("foreach")
# if(isNamespaceLoaded("iterators")) unloadNamespace("iterators")
# if(isNamespaceLoaded("parallel")) unloadNamespace("parallel")
# if(isNamespaceLoaded("RSQLite")) unloadNamespace("RSQLite")
# if(isNamespaceLoaded("ROCR")) unloadNamespace("ROCR")
# if(isNamespaceLoaded("vcd")) unloadNamespace("vcd")
# if(isNamespaceLoaded("abind")) unloadNamespace("abind")
# if(isNamespaceLoaded("foreign")) unloadNamespace("foreign")
# if(isNamespaceLoaded("randomForest")) unloadNamespace("randomForest")
