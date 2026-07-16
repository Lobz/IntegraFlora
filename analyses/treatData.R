if(!require(integraFlora)) devtools::load_all()
require(plantR)
source("config.R")

load("data-tmp/treated_data_all.RData")

if(SUBSETTOPROVINCE) {
    treated_data <- lapply(treated_data, subsetToProvince)
}

treated_data <- lapply(treated_data, plantRWorkflow_part2)
save(treated_data, file="data-tmp/treated_data_part2.RData")

# Join
print("Joining in a single data.frame...")
corpus <- treated_data[[1]]
for(x in treated_data[2:length(treated_data)]) {
    corpus <- dplyr::bind_rows(corpus, x)
}

print("Saving...")
save(corpus, file="data-tmp/corpus-full.rda")
