if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
library(parallel)
library(sf)

source("config.R")

if(!dir.exists("results")) dir.create("results")
if(!dir.exists("results/total")) dir.create("results/total")

# Data about UCs from CNUC
print("Loading conservation units data...")
ucs <- read.csv("data-input/Locations/info/Summary.csv")
ucs <- ucs[grep(STATEPROVINCE, ucs$stateProvince),]

# Make a summary table
ucs$NumRecords <- NA
ucs <- ucs[,c("name", "slug", "NumRecords")]
ucs$NumOuro <- NA
ucs$NumPrata <- NA
ucs$NumBronze <- NA

# Lookup what are the names of UCs in plantR
LT <- read.csv("data-input/Locations/extraTables/uc_locstrings.csv")
LT <- LT[LT$uc_name %in% ucs$name, ]
if (nrow(LT) > 0) {
    LT[LT==""] <- NA
    loc1 <- aggregate(LT$loc.correct, list(Nome_UC = LT$uc_name), function(x) paste(x, collapse="|"))
    LT <- na.omit(LT)
    loc2 <- aggregate(LT$loc.extra, list(Nome_UC = LT$uc_name), function(x) paste(unique(x), collapse="|"))
    LT <- rbind(loc1, loc2)
    tail(LT)
    loc3 <- aggregate(LT$x, list(Nome_UC = LT$Nome_UC), function(x) paste(x, collapse="|"))
    rownames(loc3) <- loc3$slug <- slug(loc3$Nome_UC)
} else {loc3 <- list(slug="")}

# Read table of alternative names and locality names
checkedLocations <- read.csv("data-input/Locations/extraTables/checkedLocations.csv")
checkedLocations$slug <- toupper(slug(standardize_uc_name(checkedLocations$Nome_UC)))
checkedLocations <- subset(checkedLocations, slug %in% ucs$slug)

# add oficial names
officialNames <- data.frame(Nome_UC = standardize_uc_name(ucs$name), Municipio="QUALQUER", Nome_Alternativo = ucs$name, Relação = "Igual", Confiança = "Ouro", slug = ucs$slug)
LT <- rbind(checkedLocations, officialNames)

# Generate string for regex grepl in locality data
LT$uc_strings <- paste0("(",generate_uc_string(LT$Nome_Alternativo),")")
# Summarize alternative names
LT <- aggregate(LT$uc_strings, list(slug = LT$slug, Municipio = LT$Municipio, relationship = LT$Relação, confidenceLocality = LT$Confiança), function(x) paste(unique(x), collapse="|"))

# Temporary
LT <- subset(LT, confidenceLocality == "Ouro")

# Pre-treated data from GBIF, REflora and JABOT
# load("data-tmp/reflora_gbif_jabot_splink_saopaulo.RData")
print("Loading occurrence data...")
load("data-tmp/corpus.rda")
if(!"recordID" %in% names(corpus)) corpus$recordID <- 1:nrow(corpus)
if(exists("COUNTRY")) corpus <- subset(corpus, country.correct %in% COUNTRY)
if(exists("STATEPROVINCE")) corpus <- subset(corpus, stateProvince.correct %in% STATEPROVINCE | is.na(stateProvince.correct))

# Which occs are associated with each UC
print("Searching with plantR locs...")
occs_plantr <- sapply(ucs$slug, function(s) {
    if(!s %in% loc3$slug) {
        return(FALSE)
    } else {
        grepl(loc3[s, "x"], corpus$loc.correct, perl=T)
    }
}, USE.NAMES = TRUE, simplify = FALSE)

# Use regex to look for more occs
print("Searching with regex...")
occs_string_mun <- pairwiseMap(LT$x, LT$Municipio, function(str, mun) {
    if(mun=="QUALQUER") {
        res <- searchLoc(str, corpus)
    } else {
        in_mun <- which(corpus$municipality.correct == mun)
        res <- rep(FALSE, nrow(corpus))
        res[in_mun] <- searchLoc(str, corpus[in_mun, ])
    }
    res
}, simplify = FALSE)
# Combine positive matches from different municipalities
occs_string <- sapply(unique(LT$slug), function(n) {Reduce("|", occs_string_mun[LT$slug==n])}, simplify = FALSE, USE.NAMES = TRUE)
# Combine matches from occs_plantr and occs_string
occs_locality <- pairwiseMap(occs_plantr[names(occs_string)], occs_string, FUN=function(x,y) {x|y})
names(occs_locality) <- names(occs_string)

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
(sample_size = nrow(ucs))

# Shape data
print("Loading multipolygons...")
shapes <- st_read("data-input/Locations/shapes/cnuc_2025_08/cnuc_2025_08.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$slug <- slug(standardize_uc_name(shapes$nome_uc))
shapes <- subset(shapes, slug %in% ucs$slug)
shapes <- shapes[order(shapes$slug), ]

# Data with valid coordinates: either original coordinates or locality
print("Selecting and correcting valid georeferenced points (original coords) ...")
coords_original <- subset(corpus, origin.coord == "coords_original")
if(nrow(coords_original) > 0) {
    coords_original <- st_as_sf(coords_original, coords = c("decimalLongitude.new", "decimalLatitude.new"))
    coords_original <- fixDatum(coords_original) # Unify and convert datum to match SIRGAS 2000
    print("Intersecting points and shapes (original coords) ...")
    points_ucs_original <- st_intersects(shapes, coords_original)
    names(points_ucs_original) <- shapes$slug
} else {
    points_ucs_original <- as.list(rep(FALSE, nrow(shapes)))
    names(points_ucs_original) <- shapes$slug
}

print("Selecting and correcting valid georeferenced points (gazet coords) ...")
coords_gazet <- subset(corpus, resolution.gazetteer == "locality")
if(nrow(coords_gazet) > 0) {
    coords_gazet <- st_as_sf(coords_gazet, coords = c("longitude.gazetteer", "latitude.gazetteer"))
    st_crs(coords_gazet) <- "EPSG:4674" # Assumes datum is SIRGAS 2000 (used by IBGE)
    print("Intersecting points and shapes (gazet coords) ...")
    points_ucs_gazet <- st_intersects(shapes, coords_gazet)
    names(points_ucs_gazet) <- shapes$slug
} else {
    points_ucs_gazet <- FALSE
}


# Get intersection table
print("Reading intersection table...")
intersecUCs <- read.csv("data-input/Locations/extraTables/intersecUCs.csv")
# Attribute confidence based on intersections
intersecUCs$confidence <- ifelse(intersecUCs$prop > 98, "High",
                             ifelse(intersecUCs$status == "covered_buffer" | intersecUCs$prop > 80, "Medium", "Low"))
intersecUCs$slug <- slug(standardize_uc_name(intersecUCs$nome_uc))
intersecUCs$slug2 <- slug(standardize_uc_name(intersecUCs$outra_uc))

intersecUCs <- subset(intersecUCs, slug2 %in% ucs$slug)

for(i in 1:sample_size){
tryCatch({

    uc_data <- ucs[i,]
    print("Getting data for UC:")
    print(uc_data[1])
    UC <- uc_data$slug
    nome_file <- UC

    # Which records are in the gps shp
    if(any(points_ucs_original[[UC]])) {
        rcs_intersect <- coords_original$recordID[points_ucs_original[[UC]]]
        gps_original <- corpus$recordID %in% rcs_intersect
    } else gps_original <- FALSE
    if(any(points_ucs_gazet[[UC]])) {
        rcs_intersect <- coords_gazet$recordID[points_ucs_gazet[[UC]]]
        gps_gazet <- corpus$recordID %in% rcs_intersect
    } else gps_gazet <- FALSE
    gps_both <- gps_original & gps_gazet

    # Generate string for regex grepl in locality data
    intersected <- subset(intersecUCs, slug == UC)
    high <- intersected$outra_uc[intersected$confidence=="High"]
    medium <- intersected$outra_uc[intersected$confidence=="Medium"]

    # Exact UC name
    occs_uc_name <- occs_locality[[UC]]
    locality_exact <- occs_string[[UC]]
    plantr_exact <- occs_plantr[[UC]]

    intersect_high <- Reduce('|', occs_locality[high])
    intersect_medium <- Reduce('|', occs_locality[medium])

    if(length(high)==0){
        intersect_high <- FALSE
    }
    if(length(medium)==0){
        intersect_medium <- FALSE
    }


    # Join all filters
    occs_total <- occs_uc_name | gps_original | gps_gazet | intersect_high | intersect_medium
    if(!any(occs_total)) {
        print("No records found for CU:")
        print(UC)

        ucs[i,3:ncol(ucs)] <- 0

        next
    }

    # What criteria was used to select each record
    corpus$selectionCategory <- NA
    corpus$selectionCategory[gps_original] <- "coords_original"
    corpus$selectionCategory[gps_gazet] <- "coords_gazet"
    corpus$selectionCategory[gps_both] <-  "coords_both"
    corpus$selectionCategory[intersect_medium] <-  "intersect_medium"
    corpus$selectionCategory[intersect_high] <- "intersect_high"
    corpus$selectionCategory[plantr_exact] <- "plantr_exact"
    corpus$selectionCategory[locality_exact] <- "locality_exact"

    # What quality is the locality
    corpus$confidenceLocality <- "Low" # original GPS data
    corpus$confidenceLocality[intersect_medium | gps_gazet] <- "Medium"
    corpus$confidenceLocality[occs_uc_name | intersect_high | gps_both] <- "High"

    total <- corpus[occs_total,]

    total$Nome_UC <- uc_data$name
    save(total, file=paste0("results/total/",nome_file,".rda"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    ucs[i,]$NumOuro <- sum(total$confidenceLocality=="High")
    ucs[i,]$NumPrata <- sum(total$confidenceLocality=="Medium")
    ucs[i,]$NumBronze <- sum(total$confidenceLocality=="Low")

}, error = function(e) {
    warning(e)
})
}

ucs$nome_file <- NULL
ucs$slug <- NULL

# Save summary
write.csv(ucs, "results/summary_getOccs.csv", row.names=FALSE)
summary(ucs==0)
summary(ucs<20)
