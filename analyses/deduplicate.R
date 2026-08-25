if(!require(integraFlora)) devtools::load_all()
library(plantR)

load(file.path(Sys.getenv("DATATMP"),"corpus-full.rda"))

print("Removing duplicates...")
loc.names <- c(loc.cols, paste0(loc.cols, ".new"), "longitude.gazetteer", "latitude.gazetteer")
names(loc.names) <- loc.names
loc.names <- c(loc.str = "loc.correct", res.gazet = "resolution.gazetteer", res.orig =
    "resol.orig", loc.check = "loc.check", loc.names)

my_valDup <- function(x) validateDup(x, noNumb = NA, noYear = NA, noName = NA, prop=0.6,
  comb.fields = list(
    c("family", "col.last.name", "col.number", "col.loc"),
    c("family", "col.last.name", "col.number", "col.year"),
    # c("family", "col.year", "col.number", "col.loc"),
    c("species", "col.last.name", "col.number", "col.year"),
    c("species", "col.last.name", "col.number", "col.loc"),
    c("col.year", "col.last.name", "col.number", "col.loc")),
  tax.names = c(family = "family.new", species = "scientificName.new", tax.auth =
    "scientificNameAuthorship.new", det.name = "identifiedBy.new", det.year =
    "yearIdentified.new", tax.check = "tax.check", tax.rank = "taxon.rank", status =
    "scientificNameStatus", id = "id", name.full = "scientificNameFull", gen = "genus.new", sp = "species.new"),
  geo.names = c(lat = "decimalLatitude.new", lon = "decimalLongitude.new", org.coord =
    "origin.coord", prec.coord = "precision.coord", geo.check = "geo.check", datum = "geodeticDatum"),
  loc.names = loc.names, ignore.miss = T)

badnames <- names(corpus)[endsWith(names(corpus), "1")]
if(length(badnames) > 0) {
  warning(paste("Removing problematic columns:", paste(badnames, collapse=", ")))
  corpus[,badnames] <- NULL
}
corpus <- my_valDup(corpus)

print("Saving...")
save(corpus, file=file.path(Sys.getenv("DATATMP"), "corpus.rda"))
