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

# Prepare for searching alternative names in the occurrence data
altNames <- consolidateNamesTable(checkedLocations = read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = unique(unlist(lapply(info_data_list, function(x) unique(x$name)))))

print("Merging info...")
dt <- merge_many(info_data_list, filenames, searchWith = altNames)

dt <- dt[order(dt$name),]

write.csv(dt, file.path(results_dir, "UCsummary.csv"), row.names=F)

print("Loading multipolygons...")
shapes <- readShapeInfo(remove_extra_cols = TRUE)

lapply(shapes, names)
filenames_shapes <- sapply(shapes, function(x) unique(x$source))


info_from_shapes <- lapply(shapes, function(x) {
    x$geometry <- NULL
    as.data.frame(x)
})
info_from_shapes <- lapply(info_from_shapes, cleanLocFields, removeDuplicates = FALSE)

dim(info_from_shapes[[1]])
dim(shapes[[1]])

shapes <- lapply(1:length(shapes), function(i) {
    if(nrow(info_from_shapes[[i]]) != nrow(shapes[[i]])) {
        stop(paste("Number of rows in info_from_shapes and shapes do not match for", filenames_shapes[i]))
    }
    shapes[[i]][, c("country", "stateProvince", "municipality")] <- info_from_shapes[[i]][, c("country", "stateProvince", "municipality")]
    shapes[[i]]
})

sapply(shapes, function(x) tab(x$stateProvince))

dtshapes <- merge_many(info_from_shapes, filenames_shapes, searchWith = altNames)
dim(dtshapes)
head(dtshapes)

dtf <- merge_info(dt, dtshapes, searchWith = altNames)

dim(dt)

dim(dtf)
setdiff(dtf$slug, dt$slug)
setdiff(dt$slug, dtf$slug)
