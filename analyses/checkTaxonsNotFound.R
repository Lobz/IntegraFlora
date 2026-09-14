
if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

taxons <- read.csv(file.path(results_dir, "taxons_not_found.csv"))

head(taxons)

fullGetId <- function(x) {

    print("Formatting taxonomy...")
    x <- completeScientificName(x, rm.miss = T)
    x <- removeRepeatedAuthorship(x)

    if(!exists("bfoNamesBryophyta")){
        print("Loading bryophyta and algae...")
        data(list = c("bfoNamesBryophyta", "bfoNamesAlgae"), package = "plantRdata")
    }
    tax <- dplyr::full_join(bfoNamesBryophyta, bfoNamesAlgae)
    tax <- dplyr::full_join(tax, plantR::bfoNames)
    x <- getTaxonId(x, db = tax)

    x <- tryAgain(x, not_found, function(x) {
        x <- isolateAuthorship(x)
        x <- getTaxonId(x, db = tax)
    })

    # We'll try getting extra taxons with wfo
    # loading the WFO and WCVP backbones
    if(!exists("wfoNames")) {
        print("Loadind world flora databases...")
        data(list = c("wfoNames", "wcvpNames"), package = "plantRdata")
    }
    # using the World Flora Online
    x <- tryAgain(x, not_found, getTaxonId, db = wfoNames)
    # using the World Checklist of Vascular Plants
    x <- tryAgain(x, not_found, getTaxonId, db = wcvpNames)
}

x <- fullGetId(taxons)
tab(x$tax.notes)

names(x)[4] <- "Freq"

write.csv(x[,1:4], file.path(results_dir, "taxons_not_found.csv"), row.names=F)

nf <- x[not_found(x), ]
write.csv(nf[,1:4], file.path(results_dir, "taxons_not_found.csv"), row.names=F)

x <- x[found(x), ]
world <- startsWith(x$id, "wfo") | startsWith(x$id, "wcvp")
head(x[world,])

matched_back <- x[!world,]
head(matched_back)

not_bfo <- x[world,]

x <- matched_back
write.csv(not_bfo, file.path(results_dir, "taxons_not_bfo.csv"))
write.csv(x, file.path(results_dir, "taxons_matched_back.csv"))

# Check what's in the results

nf_lists <- list.files(file.path(results_dir, "checklist/"), pattern = ".*_nomesInvalidos.csv", full.names = T)
ni <- lapply(nf_lists, read.csv)
ni <- do.call(rbind, ni)

y <- reverseListFormatting(ni)

y <- fullGetId(y)

tab(y$tax.notes)

setdiff(x$scientificName, y$scientificName)
setdiff(y$scientificName, x$scientificName)
