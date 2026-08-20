if(!require(integraFlora)) devtools::load_all()
require(plantR)

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

save(reflora,file="data-tmp/reflora.RData")
