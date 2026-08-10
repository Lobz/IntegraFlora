if(!require(integraFlora)) devtools::load_all()
require(plantR)
source("config.R")

load("data-tmp/all_data.RData")

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
save(treated_data, file="data-tmp/treated_data_all.RData")
