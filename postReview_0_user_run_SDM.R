# File: postReview_0_user_run_SDM.r
# Purpose: recreate metadata after model review and export 
# in the Mobi project

library(here)
rm(list=ls())
loc_scripts <- here()

#######
###  loop it
######
rootPath <- file.path("H:","spp_models", "_test")

fldrs <- list.dirs(rootPath, recursive = FALSE)

# if you want to run fewer than the full list of folders
# subset here. 
#fldrs <- fldrs[c(3,6)]
#



sppVec <- unlist(lapply(strsplit(fldrs, split = "/"), FUN = function(x) x[[4]]))
sppVec

for(sv in 1:length(sppVec)){

  loc_model <- fldrs[[sv]]
  model_species <- sppVec[[sv]]
  load(file.path(loc_model, "outputs","rdata",paste0(model_species,"_runSDM_paths.Rdata")))
  # the vars we need
  fn_arg_vars <- c("project_overview","model_comments","metaData_comments","modeller","project_blurb","modelrun_meta_data")
  for(arg in fn_arg_vars)
    assign(arg, fn_args[[arg]])
  
  nm_refBoundaries <- file.path(loc_scripts,"_data","other_spatial","feature", "US_States.shp")
  nm_db_file <- file.path(loc_scripts,"_data", "databases", "SDM_lookupAndTracking.sqlite")

  source(file.path(loc_scripts,"postReview_createMetadata.r"))

}

