if(!require(integraFlora)) devtools::load_all()
require(plantR)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

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

save(splink, file=file.path(tmp_dir, "splink.rda"))
# todo: decide what to do with barcode NA