# File: 3_createModel.R
# Purpose: to create the maxent model. This includes:
# - create initial model to remove poorest performing env vars
# - validate using leave-one-out jackknifing
# - create a final model using all presence points, stratify by EO using RA
# - build partial plots of top performing env vars for metadata output

#for maxent
# give java more ram (1gb here) before loading dismo
options(java.parameters = "-Xmx5g" )
library(dismo)


outPth <- file.path(loc_model, ElementNames$Code,"outputs","ensemble_varFiltering")
dir.create(outPth, showWarnings = FALSE)

me.out <- maxent(df.full[,indVarCols], df.full[,depVarCol],
                 path = outPth)

###
# Remove the least important env vars ----
##

me.dat <- as.data.frame(slot(me.out, "results"))
me.imp.dat <- me.dat[grepl("permutation.importance",rownames(me.dat)), ,drop = FALSE]
me.imp.dat <- cbind(me.imp.dat, 
                    "var" = unlist(lapply(rownames(me.imp.dat), FUN = function(x) strsplit(x, "\\.")[[1]][[1]])))

impvals <- me.imp.dat
names(impvals) <- c("imp","var")

OriginalNumberOfEnvars <- nrow(impvals)

# first remove the bottom of the correlated vars
corrdEVs <- corrdEVs[tolower(corrdEVs$gridName) %in% impvals$var,]
if(nrow(corrdEVs) > 0 ){
  for(grp in unique(corrdEVs$correlatedVarGroupings)){
    vars <- tolower(corrdEVs[corrdEVs$correlatedVarGroupings == grp,"gridName"])
    imp.sub <- impvals[impvals$var %in% vars,, drop = FALSE]
    suppressWarnings(varsToDrop <- imp.sub[!imp.sub$imp == max(imp.sub$imp),, drop = FALSE])
    impvals <- impvals[!impvals$var %in% rownames(varsToDrop),,drop = FALSE]
  }
  rm(vars, imp.sub, varsToDrop)
}

# set the percentile, here choosing above 25% percentile
#envarPctile <- 0.25
envarPctile <- 0.50
y <- quantile(impvals$imp, probs = envarPctile)
impEnvVars <- impvals[impvals$imp > y,]
subsetNumberofEnvars <- nrow(impEnvVars)
rm(y)
# which columns are these, then flip the non-envars to TRUE
impEnvVarCols <- names(df.full) %in% impEnvVars$var
impEnvVarCols[1:5] <- TRUE
# subset!
df.full <- df.full[,impEnvVarCols]
# reset the indvarcols object
indVarCols <- c(6:length(names(df.full)))

rm(impvals, impEnvVars, impEnvVarCols)

##
# code above is for removing least important env vars
##

# subsample function. x is data frame. ssvec is sampSizeVec (not including pseu-a)
# this will get the df down to the size indicated by sampSizeVec, 
# and possibly could be used in a recipe inside the caret model (as a downsample)
# but I can't figure that out yet. 
# use it here to downsample before modeling
subSampByGp <- function(x, ssvec, gpColName) {
  df.sub = x[FALSE,]
  for(i in 1:length(ssvec)){
    cls <- names(ssvec[i])
    toDrawFrom <- x[x[,gpColName] == cls, ]
    #if(nrow(toDrawFrom) < ssvec[[i]]){
      #wimped out trying to sleuth down cases where ssvec is bigger
     # draws <- sample(1:nrow(toDrawFrom), ssvec[[i]], replace = TRUE)
    #} else {
    if(ssvec[[i]] > nrow(toDrawFrom)) {
      ssvec[[i]] <- nrow(toDrawFrom)
    }
      draws <- sample(1:nrow(toDrawFrom), ssvec[[i]], replace = FALSE)  
    #}
    
    drawnSamps <- toDrawFrom[draws,]
    df.sub <- rbind(df.sub, drawnSamps)
  }
  df.sub
}

# prep for validation loop ----
#now that entire set is cleaned up, split back out to use any of the three DFs below
df.in2 <- subset(df.full,pres == "1")
df.abs2 <- subset(df.full, pres == "0")
df.in2$stratum <- factor(df.in2$stratum)
df.abs2$stratum <- factor(df.abs2$stratum)
df.in2$group_id <- factor(df.in2$group_id)
df.abs2$group_id <- factor(df.abs2$group_id)
df.in2$pres <- factor(df.in2$pres)
df.abs2$pres <- factor(df.abs2$pres)

#reset the row names, needed for random subsetting method of df.abs2, below
row.names(df.in2) <- 1:nrow(df.in2)
row.names(df.abs2) <- 1:nrow(df.abs2)

# reduce the number of validation loops if more than 50. 50 is plenty!
# randomly draw to get the validation set.
if(length(group$vals) > 50) {
  group$vals <- sample(group$vals, size = 50)
  group$vals <- factor(group$vals)
} 

##initialize the Results vectors for output from the jackknife runs
me.trRes <- vector("list",length(group$vals))
names(me.trRes) <- group$vals[]
me.evSet <- vector("list",length(group$vals))
names(me.evSet) <- group$vals[]	   
me.evRes <- vector("list",length(group$vals))
names(me.evRes) <- group$vals[]
me.t.f <- vector("list",length(group$vals))
names(me.t.f) <- group$vals[]
me.t.ctoff <- vector("list",length(group$vals))
names(me.t.ctoff) <- group$vals[]
me.v.rocr.rocplot <- vector("list",length(group$vals))
names(me.v.rocr.rocplot) <- group$vals[]
me.v.rocr.auc <- vector("list",length(group$vals))
names(me.v.rocr.auc) <- group$vals[]
me.v.y <- vector("list",length(group$vals))
names(me.v.y) <- group$vals[]
me.v.kappa <- vector("list",length(group$vals))
names(me.v.kappa) <- group$vals[]
me.v.tss <- vector("list",length(group$vals))
names(me.v.tss) <- group$vals[]
me.v.OvAc <- vector("list",length(group$vals))
names(me.v.OvAc) <- group$vals[]
me.t.importance <- vector("list",length(group$vals))
names(me.t.importance) <- group$vals[]
me.t.rocr.pred <- vector("list",length(group$vals))
names(me.t.rocr.pred) <- group$vals[]
me.v.rocr.pred <- vector("list",length(group$vals))
names(me.v.rocr.pred) <- group$vals[]

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
    me.evSet[[i]] <- do.call("subset",list(df.in2, evSelStr))
    # use sample to grab a random subset from the background points
    BGsampSz <- nrow(me.evSet[[i]])
    ## fix outlier where me.evSet is bigger than absence set
    if(BGsampSz > nrow(df.abs2)) BGsampSz <- nrow(df.abs2)/2
    me.evSetBG <- df.abs2[sample(nrow(df.abs2), BGsampSz , replace = FALSE, prob = NULL),]
    # get the other portion for the training set
    TrBGsamps <- attr(me.evSetBG, "row.names") #get row.names as integers
    trSetBG <-  df.abs2[-TrBGsamps,]  #get everything that isn't in TrBGsamps
    # join em, clean up
    trSet <- rbind(trSet, trSetBG)
    trSet[,group$colNm] <- factor(trSet[,group$colNm])
    me.evSet[[i]] <- rbind(me.evSet[[i]], me.evSetBG)
    
    ssVec <- sampSizeVec[!names(sampSizeVec) == group$vals[[i]]]
    # re-calc pseudo-absence samples to match input training samples
    ssVec["pseu-a"] <- sum(ssVec[!names(ssVec) %in% "pseu-a"])
    
    rm(trSelStr, evSelStr, trSetBG, me.evSetBG, TrBGsamps, BGsampSz )
    
    # reduce training set based on sampsizevec
    trSet.s <- subSampByGp(trSet, ssVec[-grep("pseu-a", names(ssVec))], group$colNm)
    trSet.s <- rbind(trSet.s, trSet[trSet$pres == 0,])
    
    # create the model!
    me.trRes[[i]] <- maxent(trSet.s[,indVarCols], trSet.s[,depVarCol])
    
    # run a maxent predict on the validation data
    me.evRes[[i]] <- predict(me.trRes[[i]], me.evSet[[i]], type="prob")
    # use ROCR to structure the data. Get pres col of me.evRes (= named "1")
    me.v.rocr.pred[[i]] <- prediction(me.evRes[[i]],me.evSet[[i]]$pres)
    # extract the auc for metadata reporting
    me.v.rocr.auc[[i]] <- performance(me.v.rocr.pred[[i]], "auc")@y.values[[1]]
    cat("finished run", i, "of", length(group$vals), "\n")
  }
  
  # restructure validation predictions so ROCR will average the figure
  me.v.rocr.pred.restruct <- me.v.rocr.pred[[1]]
  #send in the rest
  for(i in 2:length(me.v.rocr.pred)){
    me.v.rocr.pred.restruct@predictions[[i]] <- me.v.rocr.pred[[i]]@predictions[[1]]
    me.v.rocr.pred.restruct@labels[[i]] <- me.v.rocr.pred[[i]]@labels[[1]]
    me.v.rocr.pred.restruct@cutoffs[[i]] <- me.v.rocr.pred[[i]]@cutoffs[[1]]
    me.v.rocr.pred.restruct@fp[[i]] <- me.v.rocr.pred[[i]]@fp[[1]]
    me.v.rocr.pred.restruct@tp[[i]] <- me.v.rocr.pred[[i]]@tp[[1]]
    me.v.rocr.pred.restruct@tn[[i]] <- me.v.rocr.pred[[i]]@tn[[1]]
    me.v.rocr.pred.restruct@fn[[i]] <- me.v.rocr.pred[[i]]@fn[[1]]
    me.v.rocr.pred.restruct@n.pos[[i]] <- me.v.rocr.pred[[i]]@n.pos[[1]]
    me.v.rocr.pred.restruct@n.neg[[i]] <- me.v.rocr.pred[[i]]@n.neg[[1]]
    me.v.rocr.pred.restruct@n.pos.pred[[i]] <- me.v.rocr.pred[[i]]@n.pos.pred[[1]]
    me.v.rocr.pred.restruct@n.neg.pred[[i]] <- me.v.rocr.pred[[i]]@n.neg.pred[[1]]
  }
  
  # run a ROC performance with ROCR
  me.v.rocr.rocplot.restruct <- performance(me.v.rocr.pred.restruct, "tpr","fpr")
  # send it to perf for the averaging lines that follow
  me.perf <- me.v.rocr.rocplot.restruct
  rm(me.v.rocr.rocplot.restruct)
  ## for infinite cutoff, assign maximal finite cutoff + mean difference
  ## between adjacent cutoff pairs  (this code is from ROCR)
  if (length(me.perf@alpha.values)!=0) me.perf@alpha.values <-
    lapply(me.perf@alpha.values,
           function(x) { isfin <- is.finite(x);
           x[is.infinite(x)] <-
             (max(x[isfin]) +
                mean(abs(x[isfin][-1] -
                           x[isfin][-length(x[isfin])])));
           x[is.nan(x)] <- 0.001; #added by tgh to handle vectors length 2
           x})
  
  for (i in 1:length(me.perf@x.values)) {
    ind.bool <- (is.finite(me.perf@x.values[[i]]) & is.finite(me.perf@y.values[[i]]))
    if (length(me.perf@alpha.values) > 0)
      me.perf@alpha.values[[i]] <- me.perf@alpha.values[[i]][ind.bool]
    me.perf@x.values[[i]] <- me.perf@x.values[[i]][ind.bool]
    me.perf@y.values[[i]] <- me.perf@y.values[[i]][ind.bool]
  }
  me.perf.sampled <- me.perf
  
  # create a list of cutoffs to interpolate off of
  alpha.values <- rev(seq(min(unlist(me.perf@alpha.values)),
                          max(unlist(me.perf@alpha.values)),
                          length=max(sapply(me.perf@alpha.values, length))))
  # interpolate by cutoff, values for y and x
  for (i in 1:length(me.perf.sampled@y.values)) {
    me.perf.sampled@x.values[[i]] <-
      approxfun(me.perf@alpha.values[[i]],me.perf@x.values[[i]],
                rule=2, ties=mean)(alpha.values)
    me.perf.sampled@y.values[[i]] <-
      approxfun(me.perf@alpha.values[[i]], me.perf@y.values[[i]],
                rule=2, ties=mean)(alpha.values)
  }
  
  ## compute average curve
  me.perf.avg <- me.perf.sampled
  me.perf.avg@x.values <- list(rowMeans( data.frame( me.perf.avg@x.values)))
  me.perf.avg@y.values <- list(rowMeans( data.frame( me.perf.avg@y.values)))
  me.perf.avg@alpha.values <- list( alpha.values )
  
  for(i in 1:length(group$vals)){
    ### get threshold
    # get me.MTP: minimum training presence (minimum votes recieved [probability]
    # for any training point)
    me.MTP <- me.trRes[[i]]@results["Minimum.training.presence.Cloglog.threshold",]
    me.maxSSS <- me.trRes[[i]]@results["Maximum.training.sensitivity.plus.specificity.Cloglog.threshold",]
    
    # calculations fail if me.MTP = 0 so if it does, fall back to me.maxSSS
    if(me.MTP == 0) {
      # max sensitivity plus specificity (me.maxSSS per Liu et al 2016)
      me.cutval <- me.maxSSS
    } else {
      me.cutval <- me.MTP
    }
    
    #apply the cutoff to the validation data
    v.me.pred.cut <- ifelse(predict(me.trRes[[i]], me.evSet[[i]]) > me.cutval,1,0)
    v.me.pred.cut <- factor(v.me.pred.cut,levels = c("1","0"))
    #make the confusion matrix
    me.v.y[[i]] <- table(observed = me.evSet[[i]][,"pres"],
                      predicted = v.me.pred.cut)
    #add estimated accuracy measures
    me.v.y[[i]] <- cbind(me.v.y[[i]],
                      "accuracy" = c(me.v.y[[i]][1,1]/sum(me.v.y[[i]][1,]), me.v.y[[i]][2,2]/sum(me.v.y[[i]][2,])))
    #add row, col names
    rownames(me.v.y[[i]])[rownames(me.v.y[[i]]) == "0"] <- "background/abs"
    rownames(me.v.y[[i]])[rownames(me.v.y[[i]]) == "1"] <- "known pres"
    colnames(me.v.y[[i]])[colnames(me.v.y[[i]]) == "0"] <- "pred. abs"
    colnames(me.v.y[[i]])[colnames(me.v.y[[i]]) == "1"] <- "pred. pres"
    print(me.v.y[[i]])
    #Generate kappa statistics for the confusion matrices
    me.v.kappa[[i]] <- Kappa(me.v.y[[i]][1:2,1:2])
    #True Skill Statistic
    me.v.tss[[i]] <- me.v.y[[i]][2,3] + me.v.y[[i]][1,3] - 1
    #Overall Accuracy
    me.v.OvAc[[i]] <- (me.v.y[[i]][[1,1]]+me.v.y[[i]][[2,2]])/sum(me.v.y[[i]][,1:2])
    ### importance measures ###
    #count the number of variables
    n.var <- length(names(me.trRes[[i]]@presence))
    
    #get permutation importance (use meanDecAcc as header for consistency)
    imp <- me.trRes[[i]]@results[grep("permutation.importance",rownames(me.trRes[[i]]@results)),]
    me.t.importance[[i]] <- data.frame("meanDecreaseAcc" = imp, "timesUsed" = NA)
    rownames(me.t.importance[[i]]) <- sub(".permutation.importance","",names(imp))
        
  } #close loop
  
  #housecleaning
  rm(trSet, me.evSet)
  
  #average relevant validation/summary stats
  # Kappa - wieghted, then unweighted
  K.w <- unlist(me.v.kappa, recursive=TRUE)[grep("Weighted.value",
                                              names(unlist(me.v.kappa, recursive=TRUE)))]
  Kappa.w.summ <- data.frame("mean"=mean(K.w), "sd"=sd(K.w),"sem"= sd(K.w)/sqrt(length(K.w)))
  K.unw <- unlist(me.v.kappa, recursive=TRUE)[grep("Unweighted.value",
                                                names(unlist(me.v.kappa, recursive=TRUE)))]
  Kappa.unw.summ <- data.frame("mean"=mean(K.unw), "sd"=sd(K.unw),"sem"= sd(K.unw)/sqrt(length(K.unw)))
  #AUC - area under the curve
  auc <- unlist(me.v.rocr.auc)
  auc.summ <- data.frame("mean"=mean(auc), "sd"=sd(auc),"sem"= sd(auc)/sqrt(length(auc)))
  #TSS - True skill statistic
  tss <- unlist(me.v.tss) 
  tss.summ <- data.frame("mean"=mean(tss), "sd"=sd(tss),"sem"= sd(tss)/sqrt(length(tss)))
  #Overall Accuracy
  OvAc <- unlist(me.v.OvAc)
  OvAc.summ <- data.frame("mean"=mean(OvAc), "sd"=sd(OvAc),"sem"= sd(OvAc)/sqrt(length(OvAc)))
  #Specificity and Sensitivity
  me.v.y.flat <- abind(me.v.y,along=1)  #collapsed confusion matrices
  me.v.y.flat.sp <- me.v.y.flat[rownames(me.v.y.flat)=="background/abs",]
  me.v.y.flat.sp <- as.data.frame(me.v.y.flat.sp, row.names = 1:length(me.v.y.flat.sp[,1]))
  specif <- me.v.y.flat.sp[,"pred. abs"]/(me.v.y.flat.sp[,"pred. abs"] + me.v.y.flat.sp[,"pred. pres"])   #specificity
  specif.summ <- data.frame("mean"=mean(specif), "sd"=sd(specif),"sem"= sd(specif)/sqrt(length(specif)))
  me.v.y.flat.sn <- me.v.y.flat[rownames(me.v.y.flat)=="known pres",]
  me.v.y.flat.sn <- as.data.frame(me.v.y.flat.sn, row.names = 1:length(me.v.y.flat.sn[,1]))
  sensit <- me.v.y.flat.sn[,"pred. pres"]/(me.v.y.flat.sn[,"pred. pres"] + me.v.y.flat.sn[,"pred. abs"])    #sensitivity
  sensit.summ <- data.frame("mean"=mean(sensit), "sd"=sd(sensit),"sem"= sd(sensit)/sqrt(length(sensit)))
  
  me.summ.table <- data.frame(metric=c("Weighted Kappa", "Unweighted Kappa", "AUC",
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
  me.summ.table
} else {
  cat("Only one polygon, can't do validation", "\n")
  me.cutval <- NA
}


# subsample down to a reasonable number 
df.full.s <- subSampByGp(df.full, sampSizeVec[-grep("pseu-a", names(sampSizeVec))], group$colNm)
df.full.s <- rbind(df.full.s, df.full[df.full$pres == 0,])

outPth <- file.path(loc_model, ElementNames$Code,"outputs","ensemble","me")
dir.create(outPth, showWarnings = FALSE)


me.out.fin <- maxent(df.full.s[,indVarCols], df.full.s[,depVarCol],
                 path = outPth,
                 args=c("-J", "-P"))
# args: (boolean flags toggle the default, and p,q,l are already 'true')
# J = Jackknife. Measure importance of each environmental variable by training with each environmental variable first omitted, then used in isolation
# P = responsecurves. Create graphs showing how predicted relative probability of occurrence depends on the value of each environmental variable
# p = product. Allow product features to be used
# q = quadratic. Allow quadratic features to be used
# l = linear. Allow linear features to be used

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
                             tot_bkgd_subsamp = nrow(df.full.s[df.full.s$pres == 0,]),
                             obs_count = nrow(df.in), 
                             bkgd_count = nrow(df.abs)
)
dbExecute(db, paste0("DELETE FROM tblModelInputs where table_code = '", baseName, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelInputs", tblModelInputs, append = TRUE)

# write validation data
me.summ.table <- cbind("model_run_name" = rep(model_run_name, nrow(me.summ.table)), 
                       "algorithm" = rep(algo, nrow(me.summ.table)), 
                       me.summ.table)

dbExecute(db, paste0("DELETE FROM tblModelResultsValidationStats where model_run_name = '", model_run_name, 
                     "' and algorithm = '", algo, "';")) # remove any previously prepped dataset entry
dbWriteTable(db, "tblModelResultsValidationStats", me.summ.table, append = TRUE)

dbDisconnect(db)
rm(db)


####
# Importance measures ----
####
# #get the importance measures
#get permutation importance (use meanDecAcc as header for consistency)
me.imp <- me.out.fin@results[grep("permutation.importance",rownames(me.out.fin@results)),]
me.f.imp <- data.frame("permutationImp" = me.imp)
rownames(me.f.imp) <- sub(".permutation.importance","",names(me.imp))

db <- dbConnect(SQLite(),dbname=nm_db_file)
# get importance data, set up a data frame
me.EnvVars <- data.frame(gridName = rownames(me.f.imp), impVal = me.f.imp, fullName="", stringsAsFactors = FALSE)
#set the query for the following lookup, note it builds many queries, equal to the number of vars
SQLquery <- paste("SELECT gridName, fullName FROM lkpEnvVars WHERE gridName COLLATE NOCASE in ('", paste(me.EnvVars$gridName,sep=", "),
					"'); ", sep="")
#cycle through all select statements, put the results in the df
for(i in 1:length(me.EnvVars$gridName)) {
  try(me.EnvVars$fullName[i] <- as.character(dbGetQuery(db, statement = SQLquery[i])[,2]))
}
##clean up

# 

# write model metadata to db

# tblModelResultsVarsUsed
varImpDB <- data.frame(model_run_name = model_run_name, 
                       algorithm = algo, 
                       gridName = tolower(envvar_list), 
                       inFinalModel = 0)
varImpDB <- merge(varImpDB, me.EnvVars[c("gridName","permutationImp")], by = "gridName", all.x = TRUE)
varImpDB$inFinalModel[!is.na(varImpDB$permutationImp)] <- 1
names(varImpDB)[names(varImpDB)=="permutationImp"] <- "impVal"
dbWriteTable(db, "tblModelResultsVarsUsed", varImpDB, append = TRUE)
dbDisconnect(db)

# partial plot data for maxent was calculated and stored as figures in the output folder

closeAllConnections()
# 

