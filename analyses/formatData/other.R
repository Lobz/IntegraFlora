
if(!require(integraFlora)) devtools::load_all()
require(plantR)

# other data
other_files <- list.files("data-input/Occurrences/OtherSources", pattern = "*.(csv|txt)", full.names = TRUE, recursive = TRUE)
if(length(other_files) > 0) {
    print("Reading other files:")
    print(other_files)
    other_data_raw <- lapply(other_files, readOccurrence)
    sizes <- sapply(other_data_raw, nrow)
    other_data_raw_positive <- other_data_raw[sizes > 0]
    print("Parsing other data...")
    other <- lapply(other_data_raw_positive, formatOccurrence)
} else {
    other <-list()
}

save(other,file="data-tmp/other.RData")
