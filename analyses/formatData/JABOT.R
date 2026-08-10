if(!require(integraFlora)) devtools::load_all()
require(plantR)

# Jabot data
jabot_files <- list.files("data-input/Occurrences/JABOT", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
if(length(jabot_files) > 0) {
    jabot_data_raw <- lapply(jabot_files, readJabot)
    jabot <- lapply(jabot_data_raw, formatJabot)
} else {
    jabot <- list()
}

save(jabot,file="data-tmp/jabot.RData")
