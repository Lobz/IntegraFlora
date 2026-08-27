if(!require(integraFlora)) devtools::load_all()
require(plantR)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# Jabot data
jabot_files <- list.files("data-input/Occurrences/JABOT", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
if(length(jabot_files) > 0) {
    jabot_data_raw <- lapply(jabot_files, readJabot)
    jabot <- lapply(jabot_data_raw, formatJabot)
} else {
    jabot <- list()
}

save(jabot,file=file.path(tmp_dir, "jabot.rda"))
