if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(sf)
library(geobr)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# We should generate a gazeteer for the UCs

# Whenever possible, we want to extract coordinates from the cnuc data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes2 <- st_read("data/raw-data/shp_cnuc_2025_08/cnuc_2025_08.shp")
shapes$nome_uc_standard <- standardize_uc_name(shapes$nome_uc)
shapes2$nome_uc_standard <- standardize_uc_name(shapes2$nome_uc)
shapes[, c("uc_id", "cd_cnuc", "pl_manejo", "cria_ato", "outro_ato", "ppgr")] <- NULL
shapes2[, c("uc_id", "cd_cnuc", "pl_manejo", "cria_ato", "outro_ato", "ppgr")] <- NULL

head(shapes)

all_ucs <- shapes

# Filter only valid shapes
is_valid <- st_is_valid(shapes)
is_valid2 <- st_is_valid(shapes2)
tab(is_valid)
tab(is_valid2)
sort(shapes$nome_uc_standard[!is_valid])
shapes$is_valid <- is_valid
sort(shapes2$nome_uc_standard[!is_valid2])
sort(shapes2$uf[!is_valid2])
shapes2$is_valid <- is_valid2

# Merge data from both sources
in_both <- intersect(shapes$nome_uc_standard[is_valid], shapes2$nome_uc_standard[is_valid2])
dim(subset(shapes, nome_uc_standard %in% in_both)) # 2962
dim(subset(shapes2, nome_uc_standard %in% in_both)) #2964

in_old <- subset(shapes, !nome_uc_standard %in% in_both)
in_new <- subset(shapes2, !nome_uc_standard %in% in_both)
common <- subset(shapes2, nome_uc_standard %in% in_both)

all_shapes <- dplyr::bind_rows(common, in_old)
all_shapes <- dplyr::bind_rows(all_shapes, in_new)

dim(all_shapes)

# We will NOT be trusting the cnuc text data for what municipalities are included in each UC
# Instead, we will use the shape data and compare it with geobr data

# Get relevant dataset info
datasets <- list_geobr()

# Read state data from geobr
state_info <- datasets[datasets[,1]=="`read_state`",]
state_latest_year <- sub(".* ","",state_info$years)
state <- geobr::read_state(year = state_latest_year)
head(state)

# Subset to SP because I don't wanna be here all day
shapes <- subset(shapes, grepl("SÃO PAULO", uf))
shapes$area_calc <- st_area(shapes)

# Intersect state shapes with UC shapes
inter_state <- st_intersection(state, shapes)
head(inter_state)
table(inter_state$name_state)

# Filter areas that are too small
inter_state$area_calc2 <- st_area(inter_state)
inter_state$area_prop <- as.numeric(inter_state$area_calc2/inter_state$area_calc)
boxplot(inter_state$area_prop~ inter_state$name_state)
save(inter_state, file="file.path(tmp_dir, 'inter_state.rda')")
load(file.path(tmp_dir, 'inter_state.rda'))

inter_state <- subset(inter_state, area_prop > 0.05)
table(inter_state$name_state, useNA="always")

table(inter_state$name_state)

# Subset to SP because I don't wanna be here all day
inter_state <- subset(inter_state, name_state == "São Paulo")

# Read munis data from geobr
munis_info <- datasets[datasets[,1]=="`read_municipality`",]
munis_latest_year <- sub(".* ","",munis_info$years)
munis <- geobr::read_municipality(year = munis_latest_year)
munis[ ,c("code_state","abbrev_state","code_region","name_region")] <- NULL
munis$area_muni <- st_area(munis)
head(munis)

# Subset to SP because I don't wanna be here all day
munis <- subset(munis, name_state == "São Paulo")
# Split into states and intersect
munis_by_state <- split(munis, munis$name_state)
str(munis_by_state)
head(munis_by_state[[1]])
names(munis_by_state)

ucs_by_state <- split(inter_state, inter_state$name_state)
head(ucs_by_state[[1]])
names(ucs_by_state)
names(munis_by_state)


inter_munis_by_state <- list()

for(i in names(ucs_by_state)) {
    try(
        inter_munis_by_state[[i]] <- st_intersection(munis_by_state[[i]], ucs_by_state[[i]])
    )
}

rbind(sapply(inter_munis_by_state, nrow), sapply(ucs_by_state, nrow))

inter_munis <- inter_munis_by_state[["São Paulo"]]

# Filter areas that are too small
inter_munis$area_calc_mun <- st_area(inter_munis)
inter_munis$area_prop_mun <- as.numeric(inter_munis$area_calc_mun/inter_munis$area_calc)
summary(inter_munis$area_prop_mun)
boxplot(inter_munis$area_prop_mun~ inter_munis$code_muni)
save(inter_munis, file=file.path(tmp_dir, 'inter_munis.rda'))
load(file.path(tmp_dir, 'inter_munis.rda'))

inter_munis <- subset(inter_munis, area_prop_mun > 0.005)
dim(inter_munis)

tab(inter_munis$nome_uc)

centroids <- st_coordinates(st_centroid(inter_munis))
inter_munis$latitude <- as.character(centroids[,2])
inter_munis$longitude <- as.character(centroids[,1])
head(inter_munis)

# inter_munis <- dplyr::bind_rows(inter_munis_by_state)
# dim(inter_munis)

# Apply plantR
head(inter_munis)


dt <- data.frame(country="Brazil", stateProvince=inter_munis$name_state,
    municipality=inter_munis$name_muni, locality=inter_munis$nome_uc,
    latitude=inter_munis$latitude, longitude=inter_munis$longitude)

dt.fix <- fixLoc(dt)
head(dt.fix)
dt.str <- strLoc(dt.fix)
head(dt.str)
loc.ideal.long <- prepLoc(dt.str$loc.string1)
dt.full <- formatLoc(dt)
table(dt.full$resolution.gazetteer)
head(dt.full)

write.csv(dt.full, "data-input/Locations/extraTables/UC_gazetteer.csv", row.names=F)
dt.full <- read.csv("data-input/Locations/extraTables/UC_gazetteer.csv", colClasses="character")
head(dt.full)

dt <- dt.full
dt <- dt[,1:6]
dt$locality <- shorten_uc_name(dt$locality)

dt.fix <- fixLoc(dt)
head(dt.fix)
dt.str <- strLoc(dt.fix)
head(dt.str)
loc.ideal.short <- prepLoc(dt.str$loc.string1)
dt.full2 <- formatLoc(dt)
table(dt.full2$resolution.gazetteer)
head(dt.full2)

table(dt.full$resolution.gazetteer,dt.full2$resolution.gazetteer)

correct.long <- dt.full$resolution.gazetteer=="locality"
correct.short <- dt.full2$resolution.gazetteer=="locality"

same.result <- dt.full$loc.correct==dt.full2$loc.correct

dt$resolution.gazetteer <- "locality"
dt$loc.ideal.long <- loc.ideal.long
dt$success.long <- correct.long
dt$loc.correct.long <- ifelse(correct.long, dt.full$loc.correct, "")
dt$loc.ideal.short <- loc.ideal.short
dt$success.short <- correct.short
dt$loc.correct.short <- ifelse(correct.short, dt.full2$loc.correct, "")

dt$loc.correct <- dt$loc.ideal.short
dt$loc.correct[correct.long] <- dt$loc.correct.long[correct.long]
dt$loc.correct[correct.short] <- dt$loc.correct.short[correct.short]
diff.loc <- correct.long & correct.short & !same.result
dt$loc.extra <-""
dt$loc.extra[diff.loc] <- dt$loc.correct.long[diff.loc]
dt$uc_name <- standardize_uc_name(dt$locality)
head(dt)

dt.base <- dt[,1:4]
dt.new <- rbind(dt.base, dt.base)
dt.new$loc <- c(loc.ideal.short, loc.ideal.long)
dt.new$loc.correct <- dt$loc.correct
dt.new$latitude <- dt$latitude
dt.new$longitude <- dt$longitude
dt.new$resolution.gazetteer <- "locality"
dt.new$source <- "cnuc_ibge"

dt.ready <- dt.new[order(dt.new$country, dt.new$stateProvince, dt.new$municipality, dt.new$locality), ]
head(dt.ready)
write.csv(dt.ready, "data-input/Locations/extraTables/ucs_locs_cnuc_shapes.csv")

write.csv(dt[correct.long | correct.short,c("uc_name", "stateProvince", "municipality", "loc.correct", "loc.extra")], "data-input/Locations/extraTables/uc_locstrings.csv")

# Check legality of gazetteer locs
gazetteer <- read.csv(file.path(tmp_dir, "gazetteer - gazetteer_new.csv"))

res_orig <- gazetteer$resolution.gazetteer
res_orig <- sub("\\|.*", "", res_orig)
res_orig <- sub("CHECK", "no_info", res_orig)
res_orig <- sub("bairro|cachoeira|distrito|localidade|mina|serra|sublocalidade|vila", "locality", res_orig)

split_locs <- as.data.frame(stringr::str_split_fixed(gazetteer$loc, "_", n=4))
colnames(split_locs)=c("country", "stateProvince", "municipality", "locality")
head(split_locs)

ret <- fixLoc(split_locs)
ret <- strLoc(ret)
ret$loc.string <- prepLoc(ret$loc.string)
ret$loc.string1 <- prepLoc(ret$loc.string1)
ret$loc.string2 <- prepLoc(ret$loc.string2)
ret <- getLoc(ret)
table(res_orig, ret$resolution.gazetteer)

ret$loc.gazet <- gazetteer$loc
ret$loc.correct.gazet <- gazetteer$loc.correct
ret$loc.best <- ret$loc.string1
ret$loc.best[is.na(ret$loc.best)] <- ret$loc.string[is.na(ret$loc.best)]
head(ret[which(res_orig!=ret$resolution.gazetteer & gazetteer$status=="ok" & res_orig=="locality"),])

checklocs <- cbind(split_locs, ret)
head(checklocs)
checklocs$resolution.expected <- res_orig
checklocs$warning <- ifelse(checklocs$loc.best != checklocs$loc.gazet, "check_loc_impossible", "")

table(res_orig!=ret$resolution.gazetteer & gazetteer$status=="ok", checklocs$loc.best != checklocs$loc.gazet & gazetteer$status=="ok")

write.csv(checklocs, "data-input/Locations/extraTables/checkloc.csv")
