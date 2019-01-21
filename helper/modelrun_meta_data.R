# modelrun_meta_data
# sources at beginning of 3rd script.

repo <- git2r::repository()
repo_head <- git2r::last_commit(repo)$sha
rm(repo)
model_start_time <- as.character(Sys.time())
sdat <- Sys.info()
model_comp_name <- sdat[['nodename']]
r_version <- R.version.string
seed_str <- gsub("[-: ]","",as.character(model_start_time))
seed <- as.integer(substr(seed_str, nchar(seed_str)-8,nchar(seed_str)))
model_run_name <- paste0(model_species, "_" , gsub(" ","_",gsub(c("-|:"),"",as.character(model_start_time))))
if (modeller == "Your name") modeller <- sdat[['effective_user']]
modelrun_meta_data <- list(model_run_name = model_run_name,
                           model_start_time=model_start_time,
                           modeller = modeller,
                           model_comp_name=model_comp_name,
                           r_version = r_version,
                           model_comments = model_comments,
                           repo_head = repo_head,
                           seed = seed)
# re-save fn_args with model_run
fn_args$modelrun_meta_data <- modelrun_meta_data
save(fn_args, file = paste0(loc_model, "/" , model_species, "/runSDM_paths.Rdata"))