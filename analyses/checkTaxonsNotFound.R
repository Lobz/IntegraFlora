
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
write.csv(x[,1:4], "results/taxons_not_found.csv", row.names=F)