if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
source("config.R")

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

# Make sure results folders exist
if(!dir.exists(file.path(results_dir, "total"))) stop('Results dir not found')
if(!dir.exists(file.path(results_dir, "checklist"))) dir.create(file.path(results_dir, "checklist"))
if(!dir.exists(file.path(results_dir, "total-treated"))) dir.create(file.path(results_dir, "total-treated"))

# Clean results dir
clean_lists(results_dir)

# Data from previous runs
done <- read.csv(file.path(results_dir, "summary_getOccs.csv"))

# Select for treating: one or more records
has_records <- done$NumRecords > 0

# Apply selection
ucs <- done[has_records, ]

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]paste0(
(sample_size = nrow(ucs))

# If using sample, I want to remove sample from done
done <- subset(done, !name %in% ucs$name)
ucs$nome_file <- slug(ucs$name)

# Space for summary
ucs$NumTaxons <- NA
ucs$NumSpecies <- NA
ucs$NumGenus <- NA
ucs$NumFamilies <- NA
ucs$NumOuro <- NA
ucs$NumPrata <- NA
ucs$NumBronze <- NA
ucs$NumLatao <- NA
ucs$NumNoMatch <- NA

# Load information for brazilian flora
if (length(list.files("data-tmp/florabr","*.rds", recursive = T))>0) {
    bf <- load_florabr(data_dir = "data-tmp/florabr")
} else {
    bf <- get_florabr(output_dir = "data-tmp/florabr")
}

for(i in 1:sample_size){
    uc_data <- ucs[i,]
    Nome_UC <- uc_data$name
    print(paste("Getting data for UC:", slug(Nome_UC)))

    nome_file <- slug(uc_data$name)

    load(file=file.path(results_dir, "total", paste0(nome_file,".rda")))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    # Order occs
    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

    # For any records missing group, we will try to find it from family
    getGroup <- function(x) {
        fam <- match(x$family.new, bf$family)
        if(any(!is.na(fam))) x$group <- bf$group[fam]
        x
    }
    total <- getGroup(total)

    write.csv(total, file.path(results_dir, "total-treated", paste0(nome_file,".csv")),  na="", row.names=FALSE)

    # Remove unwanted taxa
    groups <- c("Algae", "Bryophytes", "Fungi")
    not_accepted <- groups[c(!includeAlgae, !includeBryophyta, !includeFungi)]
    total <- subset(total, !group %in% not_accepted)
    if(nrow(total) == 0) {
        print(paste("No records for UC", Nome_UC, "after removing unwanted taxa."))
        ucs[i,]$NumTaxons <- 0
        ucs[i,]$NumSpecies <- 0
        ucs[i,]$NumGenus <- 0
        ucs[i,]$NumFamilies <- 0
        ucs[i,]$NumOuro <- 0
        ucs[i,]$NumPrata <- 0
        ucs[i,]$NumBronze <- 0
        ucs[i,]$NumLatao <- 0
        ucs[i,]$NumNoMatch <- 0
        next()
    }

    # Avoid taxons that are already represented by more detailed taxons
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)
    species <- subset(total, taxon.rank == "species")
    subspecies <- subset(total, taxon.rank < "species")
    sp <- unique(subspecies$species.new)
    if(includeSubspecies) {
        species <- dplyr::bind_rows(species, subspecies)
    } else {
        sp <- unique(species$species.new)
        # no worry about subspecies from species already represented
        subspecies <- subset(subspecies, !species.new %in% sp)
        if(nrow(subspecies) > 0) {
            # for all others, we want to get the base species
            subspecies <- formatTax(subspecies, tax.name = "species.new")
            subspecies$taxon.rank <- as.taxon.rank(subspecies$taxon.rank)
            species <- dplyr::bind_rows(species, subspecies[found(subspecies),])
            if(any(not_found(subspecies))) {
                subspecies <- subspecies[not_found(subspecies),]
                subspecies$scientificName.new <- subspecies$species.new
                subspecies$taxon.rank <- "species"
                species <- dplyr::bind_rows(species, subspecies)
            }
        }
    }
    invalid <- data.frame()
    final <- species

    if(includeGenus) {
        gen <- unique(c(species$genus.new))
        genus <- subset(total, taxon.rank == "genus" & !genus.new %in% gen)
        final <- dplyr::bind_rows(species, genus)
    } else {
        invalid <- dplyr::bind_rows(invalid, subset(total,  taxon.rank == "genus"))
    }
    if(includeFamily) {
        fam <- unique(c(species$family.new, genus$family.new))
        family <- subset(total, taxon.rank == "family" & !family.new %in% fam)
        final <- dplyr::bind_rows(final, family)
    } else {
        invalid <- dplyr::bind_rows(invalid, subset(total,  taxon.rank == "family"))
    }

    # Separate unmatched taxons
    nf <- not_found(final)
    if(any(nf)) {
        matched <- final[!nf,]
        unmatched <- final[nf,]
        unmatched$origin <- NA
        unmatched <- dplyr::bind_rows(invalid, unmatched)
        unmatched <- format_list(unmatched, Nome_UC)
        write.csv(unmatched, file.path(results_dir, "checklist", paste0(nome_file,"_nomesInvalidos.csv")), na="", row.names=FALSE)
        ucs[i,]$NumNoMatch <- nrow(unmatched)
    } else {
       matched <- final
        ucs[i,]$NumNoMatch <- 0
    }

    if(nrow(matched) == 0) {
        print(paste("No records for UC", Nome_UC, "after removing invalid taxa."))
        ucs[i,]$NumTaxons <- 0
        ucs[i,]$NumSpecies <- 0
        ucs[i,]$NumGenus <- 0
        ucs[i,]$NumFamilies <- 0
        ucs[i,]$NumOuro <- 0
        ucs[i,]$NumPrata <- 0
        ucs[i,]$NumBronze <- 0
        ucs[i,]$NumLatao <- 0
        next()
    }

    # Get info from  F&FBR
    matched$fromBFO <- startsWith(matched$id, "bfo")
    ids <- ifelse(matched$fromBFO, substr(final$id, 5, nchar(final$id)), NA)
    matches <- match(ids, bf$id)

    # Extract origin and group information
    matched$origin <-bf$origin[matches]
    matched$group <-ifelse(is.na(bf$group[matches]) | bf$group[matches] == "", matched$group, bf$group[matches])

    # Generate output file
    finalList <- format_list(matched, Nome_UC)

    # Get best records for each taxon
    tops <- top_records(finalList)
    top <- tops$top
    bottom <- tops$extra

    print(paste("Found",nrow(top),"taxons."))
    ucs[i,]$NumTaxons <- nrow(top)
    ucs[i,]$NumSpecies <- length(unique(paste(top$Gênero, top$Espécie)))
    ucs[i,]$NumGenus <- length(unique(top$Gênero))
    ucs[i,]$NumFamilies <- length(unique(top$Família))
    ucs[i,]$NumOuro <- sum(top$ConfiançaID == "Ouro")
    ucs[i,]$NumPrata <- sum(top$ConfiançaID == "Prata")
    ucs[i,]$NumBronze <- sum(top$ConfiançaID == "Bronze")
    ucs[i,]$NumLatao <- sum(top$ConfiançaID == "Latão")

    if (nrow(top) > 0) {
        top <- top[order(top$Grupos, top$Táxon_completo),]
        write.csv(top, file.path(results_dir, "checklist", paste0(nome_file, "_catalogo.csv")), na="", row.names=FALSE)
    }
    if (nrow(bottom) > 0) {
        bottom <- bottom[order(bottom$Grupos, bottom$Táxon_completo),]
        write.csv(bottom, file.path(results_dir, "checklist", paste0(nome_file, "_extra.csv")), na="", row.names=FALSE)
    }
}

# Save summary
# total <- dplyr::bind_rows(done, ucs)
total <- ucs
total$nome_file <- NULL
total <- total[order(total$name),]
total$name <- shorten_uc_name(total$name)
write.csv(total, file.path(results_dir, "summary_treatOccs.csv"), row.names=FALSE)

print("Finished.")
