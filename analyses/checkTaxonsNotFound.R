
if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data

taxons <- read.csv("results/taxons_not_found.csv")

head(taxons)

x <- completeScientificName(taxons)
# For records that have authorship inside scientific name, we want to remove that
x <- removeRepeatedAuthorship(x)
x <- getTaxonId(x)
tab(x$tax.notes)
x <- x[not_found(x), ]

data(list = c("bfoNamesBryophyta", "bfoNamesAlgae"), package = "plantRdata")
tax <- dplyr::full_join(bfoNamesBryophyta, bfoNamesAlgae)
# using the Bryophyta and Algae
x <- tryAgain(x, not_found, getTaxonId, db = tax)

x <- tryAgain(x, not_found, function(x) {
    x <- isolateAuthorship(x)
    x <- getTaxonId(x)
    # using the Bryophyta and Algae
    x <- tryAgain(x, not_found, getTaxonId, db = tax)
})

tab(x$tax.notes)

x <- x[not_found(x),]
head(x)
names(x)[4] <- "Freq"

write.csv(x[,1:4], "results/taxons_not_found.csv", row.names=F)

# We'll try getting extra taxons with wfo
# loading the WFO and WCVP backbones into a temporary environment
# using the World Flora Online
data(list = c("wfoNames", "wcvpNames"), package = "plantRdata")
x <- tryAgain(x, not_found, getTaxonId, db = wfoNames)
# using the World Checklist of Vascular Plants
x <- tryAgain(x, not_found, getTaxonId, db = wcvpNames)

tab(x$tax.notes)

x <- x[not_found(x),]
head(x)

nf <- x[not_found(x), ]
write.csv(nf[,1:4], "results/taxons_not_found.csv", row.names=F)

x <- x[found(x), ]
world <- startsWith(x$id, "wfo") | startsWith(x$id, "wcvp")
head(x[world,])

matched_back <- x[!world,]
head(matched_back)

not_bfo <- x[world,]

x <- matched_back
write.csv(not_bfo, "results/taxons_not_bfo.csv")
write.csv(x, "results/taxons_matched_back.csv")

# Check what's in the results

nf_lists <- list.files("results/checklist/", ".*_nomesInvalidos.csv", full.names = T)
ni <- lapply(nf_lists, read.csv)
ni <- do.call(rbind, ni)

y <- reverseListFormatting(ni)

setdiff(x$scientificName, y$scientificName)
setdiff(y$scientificName, x$scientificName)
