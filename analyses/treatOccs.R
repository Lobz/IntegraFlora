if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)

# Clean results folders
tt <- list.files("results/total-treated", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
checklist <- list.files("results/checklist", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
sapply(c(tt,checklist), file.remove)

# Data from previous runs
done <- read.csv("results/summary_getOccs.csv")

# Select for treating: one or more records
has_records <- done$NumRecords > 0

# Apply selection
ucs <- done[has_records, ]

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
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
    print("Getting data for UC:")
    print(Nome_UC)
    nome_file <- slug(uc_data$name)

    load(file=paste0("results/total/",nome_file,".rda"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    # Order occs
    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

    write.csv(total, paste0("results/total-treated/",nome_file,".csv"),  na="", row.names=FALSE)

    # # Detail locality quality
    # gps_orig <- total$selectionCategory == "coord_orig"
    # total$confidenceLocality[gps_orig] <- "None"
    # good_coords <- startsWith(total$geo.check, "ok_county") | startsWith(total$geo.check, "ok_locality")
    # unsure_coords <- total$geo.check %in% c("sea", "shore")
    # total$confidenceLocality[gps_orig & good_coords] <- "Medium" #todo: evaluate quality of gps polygon
    # total$confidenceLocality[gps_orig & unsure_coords] <- "Low" #todo: evaluate quality of gps polygon
    # total$confidenceLocality <- factor(total$confidenceLocality, levels = c("None", "Low", "Medium", "High"), ordered = T)


    # Separate unmatched taxons
    nf <- not_found(total)
    if(any(nf)) {
        matched <- total[!nf,]
        unmatched <- total[nf,]
        unmatched$origin <- NA
        unmatched$group <- NA
        unmatched <- format_list(unmatched, Nome_UC)
        write.csv(unmatched, paste0("results/checklist/",nome_file,"_nomesInvalidos.csv"), na="", row.names=FALSE)
        if(all(nf)) {
            continue
        }
    } else {
       matched <- total
    }

    # Avoid taxons that are already represented by more detailed taxons
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)
    subspecies <- subset(total, taxon.rank < "species")
    sp <- unique(subspecies$species.new)
    species <- subset(total, taxon.rank == "species" & !species.new %in% sp)
    gen <- unique(c(subspecies$genus.new, species$genus.new))
    genus <- subset(total, taxon.rank == "genus" & !genus.new %in% gen)
    fam <- unique(c(subspecies$family.new, species$family.new, genus$family.new))
    family <- subset(total, taxon.rank == "family" & !family.new %in% fam)

    final <- dplyr::bind_rows(subspecies, species, genus, family)

    # Get info from  F&FBR
    matched$fromBFO <- startsWith(matched$id, "bfo")
    ids <- ifelse(matched$fromBFO, substr(final$id, 5, nchar(final$id)), NA)
    matches <- match(ids, bf$id)

    # Extract origin and group information
    matched$origin <-bf$origin[matches]
    matched$group <-bf$group[matches]

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
    ucs[i,]$NumNoMatch <- nrow(unmatched)

    write.csv(top, paste0("results/checklist/",nome_file,"_modeloCatalogo.csv"), na="", row.names=FALSE)
    write.csv(bottom, paste0("results/checklist/",nome_file,"_extra.csv"), na="", row.names=FALSE)
}
ucs$nome_file <- NULL

# Save summary
total <- dplyr::bind_rows(done, ucs)
total <- total[order(total$name),]
write.csv(total, "results/summary_treatOccs.csv", row.names=FALSE)
summary(total==0)
summary(total<20)