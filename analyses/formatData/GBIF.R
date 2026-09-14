if(!require(integraFlora)) devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# GBIF data
gbif_files <- list.files("data-input/Occurrences/GBIF", pattern = "*.(zip|csv)$", full.names = TRUE, recursive = TRUE)
if(length(gbif_files > 0)) {
    print("Reading gbif files")
    print(gbif_files)
    gbif_data_raw <- lapply(gbif_files, readGBIF)
    print("Formatting gbif files")
    gbif <- lapply(gbif_data_raw, formatGBIF)
} else {
    gbif <- list()
}

save(gbif, file=file.path(tmp_dir, "gbif.rda"))
