if(!require(integraFlora)) devtools::load_all()
require(plantR)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# Reflora data
reflora_files <- list.files("data-input/Occurrences/REFLORA", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
if(length(reflora_files) > 0) {
    print("Reading reflora files:")
    print(reflora_files)
    reflora_data_raw <- lapply(reflora_files, readReflora)
    print("Parsing reflora data...")
    reflora <- lapply(reflora_data_raw, formatReflora)
} else {
    reflora <-list()
}

save(reflora,file=file.path(tmp_dir, "reflora.rda"))
