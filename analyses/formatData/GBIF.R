if(!require(integraFlora)) devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_files <- list.files("data-input/Occurrences/GBIF", pattern = "*.(zip|csv)", full.names = TRUE, recursive = TRUE)
if(length(gbif_files > 0)) {
    print("Reading gbif files")
    gbif_data_raw <- lapply(gbif_files, readGBIF)
    print("Formatting gbif files")
    gbif <- lapply(gbif_data_raw, formatGBIF)
} else {
    gbif <- list()
}

save(gbif, file="data-tmp/gbif.RData")
