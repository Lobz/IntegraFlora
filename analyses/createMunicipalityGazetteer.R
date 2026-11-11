library(plantR) # used for reading and cleaning occurrence data
library(geobr)

# Get relevant dataset info
datasets <- list_geobr()

# Read munis data from geobr
munis_info <- datasets[datasets[,1]=="read_municipality",]
munis_latest_year <- sub(".* ","",munis_info$year)
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

locs.complete <- locs.correct
locs.complete$loc <- locs.incomplete$loc.string1

# Select loc strings which are duplicated (same mun name in different states)
dups <- locs.complete$loc[duplicated(locs.complete$loc)]

# Remove duplicate names from gazetteer
locs.complete <- subset(locs.complete, !loc %in% dups)
anyDuplicated(locs.complete$municipality)

head(locs.complete)
write.csv(locs.complete, "data-input/Locations/extraTables/uniqueMunicipalities.csv", row.names = F)

# locs with state abbrev
head(locs.correct)
locs.with.state.abbrev <- locs.correct
locs.with.state.abbrev$stateAbbrev <- munis$abbrev_state[match(locs.with.state.abbrev$stateProvince, munis$name_state)]
locs.with.state.abbrev$loc <- paste0(locs.with.state.abbrev$loc, " (", tolower(locs.with.state.abbrev$stateAbbrev), ")")
head(locs.with.state.abbrev)
head(locs.complete)

# Remove unneded cols and join
locs.complete$source <- locs.with.state.abbrev$source <- tolower(munis_info$source)
desired_cols <- c("source", "loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
locs.out <- rbind(locs.complete[,desired_cols], locs.with.state.abbrev[,desired_cols])


head(locs.out)
anyDuplicated(locs.out$loc)

write.csv(locs.out, "data-input/Locations/extraTables/municipalityGazetteer.csv", row.names = F)

# Combine with plantR gazetteer to add infos
gazet = rbind(plantR:::gazetteer, locs.out)

# Test if the gazetteer works
test.out <- formatLoc(df.incomplete, gazet = gazet)

# The gazetteer is not working -> ask Renato abt this
head(test.out)
