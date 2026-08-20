library(plantR) # used for reading and cleaning occurrence data
library(geobr)

# Get relevant dataset info
datasets <- list_geobr()

# Read munis data from geobr
munis_info <- datasets[datasets[,1]=="`read_municipality`",]
munis_latest_year <- sub(".* ","",munis_info$years)
munis <- geobr::read_municipality(year = munis_latest_year)

head(munis)

# Complete df
df.correct <- data.frame(country = "Brazil", stateProvince = munis$name_state, municipality = munis$name_muni)

# Get correct loc string
locs.correct <- formatLoc(df.correct)

table(locs.correct$resolution.gazetteer)

# Extract problem cases
not.found <- (locs.correct$resolution.gazetteer != "county")
locs.not.found <- locs.correct[not.found, ]
write.csv(locs.not.found, "data-input/Locations/extraTables/municipalitites_not_found.csv", row.names = F)
locs.correct <- locs.correct[!not.found, ]

# Df with state info missing
df.incomplete <- data.frame(country = "Brazil", stateProvince = "", municipality = locs.correct$municipality, locality = "")

# Get alternative loc strings
locs.incomplete <- strLoc(fixLoc(df.incomplete))
locs.incomplete$loc.string1 <- prepLoc(locs.incomplete$loc.string1)

head(locs.incomplete)

locs.correct$loc <- locs.incomplete$loc.string1

head(locs.correct)
write.csv(locs.correct, "data-input/Locations/extraTables/uniqueMunicipalities.csv", row.names = F)

# Remove unneded cols and rename
locs.correct$source <- tolower(munis_info$source)
locs.out <- locs.correct[,c(13,8:12)]

# Select loc strings which are duplicated (same mun name in different states)
dups <- locs.out$loc[duplicated(locs.out$loc)]

# Remove duplicate names from gazetteer
locs.out <- subset(locs.out, !loc %in% dups)

head(locs.out)

write.csv(locs.out, "data-input/Locations/extraTables/municipalityGazetteer.csv", row.names = F)

# Combine with plantR gazetteer to add infos
gazet = rbind(plantR:::gazetteer, locs.out)

# Test if the gazetteer works
test.out <- formatLoc(df.incomplete, gazet = gazet)

# The gazetteer is not working -> ask Renato abt this
head(test.out)
