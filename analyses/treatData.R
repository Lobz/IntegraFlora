if(!require(integraFlora)) devtools::load_all()
require(plantR)
source("config.R")

load("data-tmp/treated_data_all.RData")

print("Subsetting...")
if(SUBSETTOPROVINCE) {
    treated_data <- lapply(treated_data, subsetToProvince)
}

print("Chunking...")
treated_data <- chunk(treated_data, chunk_size)

print("Treating data...")
treated_data <- lapply(treated_data, plantRWorkflow_part2)
print("Part 2 complete. Saving...")
save(treated_data, file="data-tmp/treated_data_part2.RData")


# Join
print("Joining in a single data.frame...")
corpus <- treated_data[[1]]
for(i in length(treated_data):2) {
    print(i)
    corpus <- join(corpus, treated_data[[i]], merge = F)
    treated_data[[i]] <- NULL
}

print("Saving...")
save(corpus, file="data-tmp/corpus-full.rda")

    # Save unmatched taxons
    nf <- corpus[corpus$tax.notes == "not found" | !startsWith(corpus$id, "bfo"), ]
    nf <- aggregate(nf$catalogNumber, list(family=nf$family, scientificName=nf$scientificName, scientificNameAuthorship=nf$scientificNameAuthorship), function(x) length(unique(x)))
    nf <- nf[order(nf$x, decreasing = T),]
    write.csv(nf[nf$x>=10,], "results/taxons_not_found.csv", row.names=F)
