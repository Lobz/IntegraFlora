devtools::load_all()
library(stringr)
library(florabr)
library(parallel)
library(sf)
library(geobr)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

# Pre-treated data from GBIF, REflora and JABOT
load(file.path(tmp_dir, "reflora_gbif_jabot_splink_saopaulo.rda"))
dt$recordID <- 1:nrow(dt) # I need a unique ID for this

# Data with valid coordinates: either original coordinates or locality
valid_coords <- subset(dt, origin.coord == "coords_original" | resolution.gazetteer == "locality" & !is.na(decimalLongitude.new))
valid_coords <- subset(valid_coords,  !is.na(as.numeric(decimalLongitude.new)))
table(valid_coords$geo.check)
valid_points <- st_as_sf(valid_coords, coords = c("decimalLongitude.new", "decimalLatitude.new"))
# Unify and convert datum to match SIRGAS 2000
valid_points <- fixDatum(valid_points)

# Get shapes for municipalities
shapes <- read_municipality("SP", year = 2024)
munis_plantR <- data.frame(country = "Brasil", stateProvince = "São Paulo", municipality = shapes$name_muni, locality = NA)
munis_plantR <- plantR::formatLoc(munis_plantR)

rownames(shapes) <- rownames(munis_plantR) <- shapes$name_muni

# Get shape for São Paulo
sp <- read_state("SP")

# Intersect points with shapes
points_muns <- st_intersects(shapes, valid_points)
names(points_muns) <- shapes$name_muni
sapply(points_muns, length)


plotMun <- function(name, plot = TRUE, save = TRUE, refdf = dt) {
    gps_filter <- points_muns[[name]]
    filtered_gps <- valid_points[gps_filter,]
    # othermuns <- unique(filtered_gps$municipality.correct)
    # plot(filtered_gps, add=T, col = "blue")
    name_filter <- which(startsWith((refdf$loc.correct), munis_plantR[name, "loc.correct"]))
    name_filter_gps <- which(startsWith((valid_points$loc.correct), munis_plantR[name, "loc.correct"]))
    filtered_name <- refdf[name_filter,]
    if(nrow(filtered_name) == 0) {
        print(paste("Zero matches:", name))
        print(sort(table(filtered_gps$municipality.correct)) )
    }

    # Summary from GPS
    total_gps <- nrow(filtered_gps)
    correct <- length(intersect(name_filter_gps, gps_filter))
    na <- sum(is.na(filtered_gps$municipality.correct))
    wrong <- total_gps - correct - na
    summ_gps <- c(total_gps=total_gps,correct=correct,wrong_name=wrong,name_not_av=na)

    # Summary from Name
    total_name <- nrow(filtered_name)
    wrong <- length(name_filter_gps) - correct
    na <- total_name - correct - wrong
    summ_name <- c(total_name=total_name,correct=correct,wrong_gps=wrong,gps_not_av=na)

    total <- total_gps + total_name
    # Plots
    if(plot && total > 0) {
        tryCatch( {

        if(save) {
            png(paste0("plots/municipios/", tolower(plantR::rmLatin(name)), ".png"), height=480, width=640)
        }
        if(total_gps > 0 && total_name > 0){
            par(mfrow=c(2,2))
        } else {
            par(mfrow=c(2,1))
        }
        if(total_name > 0) {
            plot(sp$geom, main=name)
            plot(st_geometry(shapes[name,]), add=T)
            plot(valid_points$geometry[name_filter_gps], col=rgb(0,0,1,0.1), pch = 4, add=T)
        }
        if(summ_gps["wrong_name"] > 0){
            barplot(sort(table(filtered_gps$municipality.correct[filtered_gps$municipality.correct != name]), decreasing = TRUE)[1:3], main="Top three wrong municipalities", las=1, horiz=T)
        }
        if(total_name > 0) {
            barplot(summ_name[2:4], main = "Coords of points filtered by municipality")
        }
        if(total_gps > 0){
            barplot(summ_gps[2:4], main="Municipality of points filtered by GPS")
        }
        },
        finally={
            dev.off()
        })
    }

    c(summ_gps, summ_name[c(1,3:4)])
}

plotMun("Ubatuba")
plotMun("Campinas")
plotMun("Valinhos")
plotMun("Santo André")
plotMun("Santos")
plotMun("Embu das Artes")
plotMun("Campos do Jordão")
plotMun("São Paulo")
plotMun("Alfredo Marcondes")
plotMun("São Luiz do Paraitinga")

tabs <- lapply(rownames(shapes), function(x) try(plotMun(x, plot=T)))
tabls <- sapply(tabs, function(x) if(class(x) == "integer") FALSE else TRUE)
tabs <- do.call(rbind, tabs)
rownames(tabs) <- rownames(shapes)
write.csv(tabs, file.path(results_dir, "locations/test_gps_municipalitites.csv"))
tabs <- read.csv(file.path(results_dir, "locations/test_gps_municipalitites.csv"))

t <- as.data.frame(tabs)
t$total <- t$total_gps + t$total_name - t$correct
t <- subset(t, total_name > 0)
summary(t)
ts <- subset(t, total_name > 20)
# Mean 71% and median 80%????
summary(t$correct/t$total_gps)
summary(t$correct/t$total_name)
boxplot(t$correct/t$total_gps)
summary(ts$correct/ts$total_gps)
plot(ts$correct/ts$total_gps ~ts$total_gps)
# Mean 17% and median 7% actual wrong names
summary(t$wrong_name/t$total_gps)
summary(t$wrong_name/t$total_name)
summary(t$name_not_av/t$total_gps)
summary(t$wrong_name/t$total_name)
# In total, 8.4% of gps locations are on the wrong municipality
sum(t$wrong_name)/sum(t$total_gps)

# Top records
subset(t, total > 50000)
subset(t, total < 20)
# Top correct perc (from GPS)
t$correct_perc <- t$correct / t$total_gps
hist(t$correct/t$total_gps, main="Correct / total GPS records", breaks=20)
t$X[t$total_name==0]
savePlot("plots/hist_municipios.png")
sort(t$correct_perc)
subset(t, correct_perc > .99)
subset(t, correct_perc < .001)
subset(t, total == 0)
subset(t, total_name == 0)

problem_munis <- c("guara", "sao paulo", "ribeira")

# Plot number of points per muni
names(tabs)[1]<-"name_muni"
shapes <- merge(shapes, tabs)
shapes$total <- shapes$total_gps + shapes$total_name - shapes$correct
dim(shapes)
plot(shapes[, "correct"], main = "Número de registros por município")
savePlot("plots/municipio_registros.png")
shapes$total_log <- log10(shapes$total)
shapes$correct_log <- log10(shapes$correct+1)
plot(shapes[, "correct_log"], main = "Número de registros por município (log)")
savePlot("plots/municipio_registros_log.png")
