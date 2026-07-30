if(!require(integraFlora)) devtools::load_all()
require(plantR)
source("config.R")

print("Loading all data...")
load("data-tmp/treated_data_all.RData")
rm(all_data)

print("Subsetting...")
if(SUBSETTOPROVINCE) {
    treated_data <- lapply(treated_data, subsetToProvince)
}

print("Chunking...")
treated_data <- chunk(treated_data, chunk_size)

print("Treating data and joining in a single data.frame...")
corpus <- plantRWorkflow_part2(treated_data[[1]])
max <- length(treated_data)
for(i in max:2) {
    print(i)
    corpus <- join(corpus, plantRWorkflow_part2(treated_data[[i]]), merge = F)
    treated_data[[i]] <- NULL
}

print("Saving...")
save(corpus, file="data-tmp/corpus-full.rda")

    # Save unmatched taxons
    nf <- corpus[corpus$tax.notes == "not found" | !startsWith(corpus$id, "bfo"), ]
    nf <- aggregate(nf$catalogNumber, list(family=nf$family, scientificName=nf$scientificName, scientificNameAuthorship=nf$scientificNameAuthorship), function(x) length(unique(x)))
    nf <- nf[order(nf$x, decreasing = T),]
    write.csv(nf[nf$x>=10,], "results/taxons_not_found.csv", row.names=F)
