devtools::load_all()
require(sf)
source("config.R")

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

STATE <- Sys.getenv("STATE")
if(STATE == "") STATE <- STATEPROVINCE
STATEPROVINCE <- STATE

print("Loading multipolygons...")
shapes <- readShapeInfo(remove_extra_cols = TRUE)

print("Combining shapes...")
shapes <- mergeShapes(shapes, keep_cols = c("name", "country", "stateProvince", "municipality", "protectionCategory", "UC_ID", "source", "type", "slug"), target_crs = "EPSG:4674")

# Remove invalid shapes
print("Removing invalid shapes...")
valid_shapes <- (st_is_valid(shapes))
shapes <- shapes[valid_shapes, ]

print(paste("Subsetting to state:", STATEPROVINCE))
shapes <- splitIntoStates(shapes, subsetToState = STATEPROVINCE)

tab(shapes$stateProvince)

# Figure out which shapes correspond to which UCs in the summary table
print("Formatting location fields...")
info_shapes <- shapes
info_shapes$geometry <- NULL
info_shapes <- as.data.frame(info_shapes)
info_shapes$locality <- info_shapes$name
info_shapes <- fixLocation(info_shapes)

# Load UC data
ucs <- read.csv(file.path(results_dir, "UCsummary.csv"))

# Prepare for searching alternative names in the occurrence data
checkedLocations <- read.csv("data-input/Locations/extraTables/checkedLocations.csv")
checkedLocations$slug <- toupper(slug(standardize_uc_name(checkedLocations$Nome_UC)))
altNames <- consolidateNamesTable(checkedLocations = checkedLocations, extraNames = unique((ucs$name)))

matching_names <- nameSearch(ucs = ucs$names, corpus = info_shapes, LT = altNames)
sapply(matching_names, sum)
shapes <- shapes[,c("slug", "area_calc", "source", "geometry")]
n <- shapes[matching_names[[1]],]

best_shapes <- lapply(seq_along(matching_names), function(i) {
    n <- matching_names[[i]]
    if(sum(n)==0) return(NULL)
    shp <- shapes[n, ]
    shp <- shp[!duplicated(shp$geometry), ]
    if(nrow(shp) <= 1) {
        return(shp)
    }
    asd <- sd(as.numeric(shp$area_calc))
    name <- names(matching_names)[i]
    print(paste0(nrow(shp), " different shapes for ", name, " with area sd=", asd))
    area <- max(shp$area_calc)
    if(asd > as.numeric(area/100)) {
        warning(paste0("Large divergence for ", name, ". Check sources. Slugs used: ", paste(unique(shp$slug), collapse=", ")))
    }
    un <- sf::st_union(shp)
    shp$geometry[1] <- un
    shp$source <- paste(shp$source, collapse=" - ")
    shp$slug <- name

    return(shp[1,])
})

shapes2 <- do.call(rbind,best_shapes)
shapes2 <- shapes[order(shapes$slug), ]

saveRDS(shapes2, file=file.path(results_dir, "maps.rds"))
