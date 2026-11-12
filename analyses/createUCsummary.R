devtools::load_all()
require(plantR)
source("config.R")

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

STATE <- Sys.getenv("STATE")
if(STATE == "") STATE <- STATEPROVINCE
STATEPROVINCE <- STATE

folder <- "data-input/Locations/info/"
info_files <- read.csv("data-input/Locations/info/file_descriptions.csv", na.strings = c("", "NA"))
filenames <- file.path(folder, info_files$arquivo)

# apply default values
info_files$sep[is.na(info_files$sep)] <- ","
info_files$dec[is.na(info_files$dec)] <- "."

# Read data according to file descriptions
info_data_list <- lapply(1:nrow(info_files), formatInfo)
# Subset to state and make names unique
info_data_list <- lapply(info_data_list, cleanLocFields, subsetTo = STATEPROVINCE, removeDuplicates = TRUE)

print("Merging info...")
dt <- mergeInfo(info_data_list, filenames)

dt <- dt[order(dt$name), c("name","country","stateProvince","municipality","protectionCategory","UC_ID","source","type","slug")]

write.csv(dt, file.path(results_dir, "UCsummary.csv"), row.names=F)
