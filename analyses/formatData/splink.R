if(!require(integraFlora)) devtools::load_all()
require(plantR)
# splink data
splink_files <- list.files("data-input/Occurrences/splink", pattern = "*.txt$", full.names = TRUE, recursive = TRUE)
if(length(splink_files) > 0) {
    print("Reading splink files:")
    print(splink_files)
    splink_data_raw <- lapply(splink_files, readSpLink)
    print("Formatting splink files:")
    splink <- lapply(splink_data_raw, formatSpLink)
} else {
    splink <- list()
}

save(splink, file="data-tmp/splink.RData")
# todo: decide what to do with barcode NA