if(!require(integraFlora)) devtools::load_all()
require(plantR)
require(parallel)
source("config.R")

load("data-tmp/gbif.RData")
load("data-tmp/reflora.RData")
load("data-tmp/jabot.RData")
load("data-tmp/splink.RData")
load("data-tmp/other.RData")

memory_usage()
# Join in a single list
all_data <- c(gbif, reflora, jabot, splink, other)
rm(gbif, reflora, jabot, splink, other)
print(paste("Found", sum(vapply(all_data, nrow, 0)), "records in", length(all_data), "files"))

memory_usage()
# Organize into bite-sized chunks (size from config? ~500k?)
if(!exists("chunk_size")) chunk_size<- 4e5

sizes <- vapply(all_data, nrow, 0)
# Remove empty sets
if(any(sizes == 0)) {
    warning("There are empty sets in data. Check for errors.")
    all_data <- all_data[sizes > 0]
    sizes <- sizes[sizes > 0]
}
memory_usage()
if(any(sizes > chunk_size)) {
    print("Splitting large files into chunks...")
    small <- all_data[sizes <= chunk_size]
    large <- all_data[sizes > chunk_size]
    large <- lapply(large, function(x) {
        split(x, rep(1:ceiling(nrow(x)/chunk_size), each=chunk_size))
    })
    all_data <- c(small,do.call(c, large))
    sizes <- sapply(all_data, nrow)
}
memory_usage()
if(any(sizes < chunk_size/2)) {
    print("Joining small files...")
    small <- all_data[sizes < chunk_size]
    large <- all_data[sizes >= chunk_size]
    small <- small[order(sapply(small, nrow))]
    while((l <- length(small))>1) {
        sum <- nrow(small[[1]]) + nrow(small[[l]])
        if(sum <= chunk_size) {
            small[[l]] <- join(small[[1]], small[[l]])
            small <- small[2:l]
        } else {
            large <- c(large, small[l])
            small <- small[1:(l-1)]
        }
        memory_usage()
    }
    all_data <- c(small, large)
}

print(paste("Organized", sum(sapply(all_data, nrow)), "records in", length(all_data), "chunks of", as.integer(chunk_size), "records"))
save(all_data, file="data-tmp/all_data.RData")

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
save(treated_data, file="data-tmp/treated_data_82-end.RData")
# save(treated_data, file="data-tmp/treated_data_all.RData")
