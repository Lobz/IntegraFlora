if(!require(integraFlora)) devtools::load_all()
require(plantR)
source("config.R")

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

load(file.path(tmp_dir, "all_data.rda"))

# Apply workflow
print("Treating data...")
if(PARALLEL) {
    cl <- parallel::makeCluster(CORES)
    parallel::clusterEvalQ(cl, if(!require(integraFlora)) devtools::load_all())
    treated_data <- parallel::parLapply(cl, all_data, plantRWorkflow_part1)
} else {
    treated_data <- list()
    for(i in 1:length(all_data)) {
        treated_data[[i]] <- plantRWorkflow_part1(all_data[[i]])
    }
}

print("Part 1 complete. Saving...")
save(treated_data, file=file.path(tmp_dir, "treated_data_all.rda"))
