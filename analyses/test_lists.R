if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# The point of this is to create and compare lists from different techniques
# For this exercise, we are using data about Estação Ecológica de Avaré
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")

# Create list using plantR, GBIF, Reflora, splink
UC_de_interesse <- "ESTAÇÃO ECOLÓGICA DE AVARÉ"
uc_string <- "e(st(a[çc?][aã?]o|)?)?.? e(col([óo?]gica)?)?.?( de)? avar[ée?]"

UC_de_interesse <- "Campos do Jordão"
uc_data <- subset(ucs, grepl(UC_de_interesse, Nome.da.UC, ignore.case=T))[3,]
Nome_UC <- uc_data$Nome.da.UC
nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))
# fix a couple common misspellings
Nome_UC <- sub("AREA", "ÁREA", Nome_UC, fixed = T)
Nome_UC <- sub("PATRIMONIO", "PATRIMÔNIO", Nome_UC, fixed = T)
uc_string <- generate_uc_string(Nome_UC)
county <- str_squish(gsub("\\(.*\\)","",uc_data$Municípios.Abrangidos))
county_splink <- paste(county, rmLatin(county))
county_plantr <- tolower(rmLatin(county))

# Splink data
splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
splink_raw <- rspeciesLink(
    Scope = "p", # this should filter out animals, but unreliable in filtering out fungi, bacteria, etc
    stateProvince = "Sao Paulo", county = county_splink,
    key = splinkkey,
    save = TRUE, dir = "data/", filename = "splink_county", MaxRecords = 2000)
dim(splink_raw)
table(splink_raw$kingdom, useNA="always")
splink_raw <- subset(splink_raw, kingdom == "Plantae")
table(splink_raw$county)
splink_raw$downloadedFrom <- "SPLINK"
sum(grepl(uc_string, splink_raw$locality, ignore.case = T, perl = T))

# Merge and treat data
occs <- formatDwc(
    splink_data = splink_raw
    )
occs <- formatOcc(occs)
occs <- formatLoc(occs)

# join with reflora and gbif
load("data/derived-data/reflora_gbif_jabot_saopaulo.rda")
occs <- dplyr::bind_rows(occs, saopaulo)


# occs <- formatCoord(occs)
# occs <- validateLoc(occs)

# Filter occs in the selected CU
occs_mun <- subset(occs, municipality.new == county_plantr)
dim(occs_mun)
horto <- subset(occs_mun, grepl("horto florestal", locality.new, ignore.case = TRUE, perl = TRUE))
table(grepl(county,ucs$Municípios.Abrangidos))
parque <- subset(occs_mun, grepl("parque", locality.new, ignore.case = TRUE, perl = TRUE))
parque <- subset(parque,!grepl("parque estadual da vassununga", locality.new, perl = TRUE)) # todo: generalize this
occs_uc_name <- subset(occs, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
table(parque$locality.new)
table(parque$municipality.new)
table(occs_uc_name$locality.new)
table(occs_uc_name$municipality.new)
dim(horto)
dim(parque)
dim(occs_uc_name) # 24
if(grepl("PARQUE",Nome_UC)) {
    total <- merge(occs_uc_name, parque, all=T)
} else {
    total <- merge(occs_uc_name, horto, all=T)
}
dim(total) # 371

# drop empty cols
total <- total[,sapply(total, function(x) !all(is.na(x)))]
dim(total)

total <- formatCoord(total)
total <- validateLoc(total)
total <- validateCoord(total) # resourse intensive - optimize?


# Some records don't have scientificName for some reason
noName <- is.na(total$scientificName)
table(noName)
# if species is present, use that
total$scientificName[noName] <- total$species[noName]
noName <- is.na(total$scientificName)
table(noName)
# else, use genus
total$scientificName[noName] <- total$genus[noName]
noName <- is.na(total$scientificName)
table(noName)
# last resort, use family
total$scientificName[noName] <- total$family[noName]
noName <- is.na(total$scientificName)
table(noName)

# Match scientificName to oficial F&FBR backbone
total <- formatTax(total)

# we're gonna try again without author (see issue #170 in plantR)
total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, use.author = F)

# What's still unmatched? Genus rank
total <- tryAgain(total, condition = function(x) x$tax.notes == "not found"& x$taxonRank=="GENUS", FUN = formatTax, tax.name = "genus")

# What's still unmatched? Vars and subspecies
total <- tryAgain(total,
    condition = function(x) x$tax.notes == "not found" & grepl("\\w+ \\w+ \\w", x$scientificName),
    FUN = function(x) {
        saved <- x$scientificName
        x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
        x <- formatTax(x)
        x$scientificName <- saved
        x
    })


# For records that have authorship inside scientific name, we want to remove that
total <- tryAgain(total,
    condition = function(x) x$tax.notes == "not found" & pairwiseMap(x$scientificNameAuthorship, x$scientificName, grepl),
    FUN = function(x) {
        saved <- x$scientificName

        x$scientificName <- str_squish(pairwiseMap(x$scientificNameAuthorship, x$scientificName, function(x,y) sub(x, "", y)))
        x <- formatTax(x)
        x
    })


# What's still unmatched? Try again with less rigor?
total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, sug.dist=0.8 )

# Finish plantR workflow
total <- validateTax(total, generalist = T)
total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)

# fix missing taxon rank
table((total$taxon.rank), useNA = "always")
table(is.na(total$taxon.rank))
total <- total[order(total$taxon.rank),]
fix_these <- which(is.na(total$taxon.rank))
x<- total$scientificName.new[fix_these]
# If there's another entry of the same taxon, get the taxon rank fron there
total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
fix_these <- which(is.na(total$taxon.rank))
x<- total$scientificName.new[fix_these]
x <- sub(" sp.","",x)
rank <- rep(NA,length(fix_these))
rank[grepl(" ",x)] <- "species"
rank[grepl(" subsp[. ]",x)] <- "subspecies"
rank[grepl(" var[. ]",x)] <- "subspecies"
rank[x == total$family.new[fix_these]] <- "family"
rank[is.na(rank)] <- "genus"
total$taxon.rank[fix_these] <- rank

# get species and genus?
total <- get_species_and_genus(total)

total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

save(total, file=paste0("data/derived-data/occs_",nome_file,".rda"))
# load(file=paste0("data/derived-data/occs_",nome_file,".rda"))

table(total$scientificName.new, total$tax.check)
table(total$scientificName.new, total$taxon.rank, useNA = "always")

summ <- summaryData(total)

table(total$basisOfRecord, useNA="always")

# Try to create my own checklist from the data treated with plantR
species <- subset(total, taxon.rank %in% c("species","subspecies","variety")) # todo: check if this is correct
    sp <- unique(species$species.new)
    gen <- unique(species$genus.new)
genus <- subset(total, !genus.new %in% gen) #TODO: add similar for family-level ids
length(unique(total$genus.new[total$taxon.rank=="genus"]))
length(unique(genus$genus.new))
final <- rbind(species, genus)

summ <- summaryData(final)

# load comparison data
load("data/raw-data/catalogoCompleto.rda")
UC_catalogo <- subset(catalogoCompleto, grepl(Nome_UC, Unidade.Conservação, perl = T, ignore.case = T))
dim(UC_catalogo)
speciesCatalogo <- unique(UC_catalogo$scientificNameFull)

compareLists <- function(l1, l2 = UC_catalogo) {
    sp1 <- unique(l1$species.new)
    sp2 <- unique(l2$species.new)
    gen1 <- unique(l1$genus.new)
    gen2 <- unique(l2$genus.new)
    f1 <- unique(l1$family.new)
    f2 <- unique(l2$family.new)

    print(c("List 1", sum(!is.na(sp1)), length(f1)))

    print("SPECIES")
    print(c(length(setdiff(sp1, sp2)), length(setdiff(sp2,sp1))))
    print("FAMILIES")
    print(c(length(setdiff(f1, f2)), length(setdiff(f2,f1))))
}

compareLists(UC_catalogo)
compareLists(final)

# compareLists(subset(final, tax.check >= "low"))
compareLists(subset(final, tax.check >= "medium"))
compareLists(subset(final, tax.check >= "high"))

# Get best records for each taxon
top <- top_records(final, n = 1)
write.csv(top, paste0(file.path(results_dir, "checklist_",nome_file,".csv"), na=""))

# Get info from  F&FBR
# get_florabr(output_dir = "data/raw-data", #directory to save the data
            # data_version = "latest", #get the most recent version available
            # overwrite = T) #Overwrite data, if it exists
bf <- load_florabr(data_dir = "data/raw-data")
ids <- substr(top$id, 5, nchar(top$id))
matches <- match(ids, bf$id)
unmatched <- which(is.na(ids))
# Try with original name (catches some errors)
matches[unmatched] <- match(top$scientificName[unmatched], bf$scientificName)
# Try with species names (helps with errors with authorship, eliminates subspecies though)
unmatched <- which(is.na(matches) & !is.na(top$species.new))
matches[unmatched] <- match(top$species.new[unmatched], bf$species)
unmatched <- which(is.na(matches))

top$scientificName[unmatched]
top$scientificNameAuthorship[unmatched]
top$scientificNameStatus[unmatched]
top$scientificNameFull[unmatched]

# Extract origin and group information
top$origin <-bf$origin[matches]
# Group is probably dependent on family innit
matches[unmatched] <- match(top$family.new[unmatched], bf$family)
top$group <-bf$group[matches]

table(top$origin)
table(top$group)
table(UC_catalogo$Origem)

# Generate output file
finalList <- format_list(top, Nome_UC)
listed <- finalList$Táxon_completo %in% UC_catalogo$Táxon
finalList[,"Já listada"] <- ifelse(listed, "Sim", "Não")

table(finalList$BD_Origem)
table(finalList$BD_Origem[!listed])
table(finalList$Localidade[!listed])
table(is.na(finalList$Barcode))

write.csv(finalList, paste0(file.path(results_dir, "checklist_",nome_file,"_modeloCatalogo.csv"), na=""))
