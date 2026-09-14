if(!require(integraFlora)) devtools::load_all()
library(geobr)

# Let's see what's going on with those stats
summ <- read.csv(file.path(results_dir, "summary_treatOccs.csv"))
# Load UC data
ucs <- read.csv("data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF))
# ucs <- read.csv("data-input/UCs.csv")

# Standardize names and reorder
ucs$Nome.da.UC <- standardize_uc_name(ucs$Nome.da.UC)
ucs <- ucs[order(ucs$Nome.da.UC), ]
ucs$area <- as.numeric(gsub("\\.","",ucs$Área.Ato.Legal.de.Criação))
ucs <- merge(ucs, summ)
ucs <- ucs[ !grepl("PARAÍSO", ucs$Nome.da.UC),]
# Type of UC
ucs$type <- factor(gsub("_.*","", slug(ucs$Nome.da.UC)))

# shapes
shapes <- sf::st_read("data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)
shapes <- subset(shapes, nome_uc %in% ucs$Nome.da.UC)
shapes <- shapes[order(shapes$nome_uc), ]
shapes$Nome.da.UC <- shapes$nome_uc

ucs$hasGeom <- ucs$Nome.da.UC %in% shapes$nome_uc
ucs$UC <- ucs$Nome.da.UC

# Open data
modCat <- list.files(file.path(results_dir, "checklist", "*.csv"), full.names = T)
nome_file_c <- sub(".*/","",modCat)
nome_file_c <- sub("_modeloCatalogo.csv","",nome_file_c)
original <- list.files(file.path(results_dir, "allfields", "*.csv"), full.names = T)
tt <- list.files(file.path(results_dir, "total-treated", "*.csv"), full.names = T)
nome_file_o <- sub(".*/","",original)
nome_file_o <- sub(".csv","",nome_file_o)

dtCat <- lapply(modCat, read.csv, na.strings = c("NA","","s.n.","s.c.","s.a."), colClasses = "character")
names(dtCat) <- nome_file_c
dtOrig <- lapply(original, read.csv, na.strings = c("NA",""), colClasses = "character")
dtTreated <- lapply(tt, read.csv, na.strings = c("NA",""), colClasses = "character")
names(dtOrig)<- names(dtTreated) <- nome_file_o
Nome.da.UC <- sapply(dtOrig, function(x) x$Nome_UC[1])

# Number of UCs with at least one record found
length(dtTreated)
# Number of records in each list
hist(summ$NumRecords[summ$NumRecords>0 & summ$NumRecords < 300000], xlab= "Número de registros", ylab = "Frequência", breaks=20, main = "Distribuição do número de registros")
savePlot("plots/NumRecords_hist.png")
hist(log10(summ$NumRecords[summ$NumRecords>0 & summ$NumRecords < 300000]), xlab= "Número de registros (log)", ylab = "Frequência", breaks=20, main = "Distribuição do número de registros")
savePlot("plots/NumRecords_hist_log.png")

# Number of records vc type
summary(ucs)
table(ucs$type)
table(ucs$NumRecords > 0, ucs$type)
table(ucs$NumRecords > 20, ucs$type)
table(ucs$NumRecords > 20)
table(ucs$NumRecords > 100, ucs$type)
table(ucs$NumRecords > 100)
table(ucs$NumRecords > 1000, ucs$type)
table(ucs$NumRecords > 10000, ucs$type)

boxplot(NumRecords ~ factor(type), data = subset(ucs, NumRecords > 0 & type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), log="y", xlab = "Tipo de UC", ylab = "Número de registros", main = "Número de registros por tipo de UC")
savePlot("plots/numRecords_log.png")
boxplot(NumRecords ~ factor(type), data = subset(ucs, type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), xlab = "Tipo de UC", ylab = "Número de registros", main = "Número de registros por tipo de UC")
savePlot("plots/NumRecords.png")

ucs[order(ucs$NumRecords),]

# NumRecords vs Area
anova(lm(ucs$NumRecords ~ ucs$area + ucs$type + ucs$hasGeom))
anova(lm(ucs$NumSpecies ~ ucs$area + ucs$type + ucs$hasGeom))
lm(ucs$area ~ ucs$type)

# Number of species
hist(ucs$NumSpecies, breaks = 20)
hist(log(ucs$NumSpecies), breaks = 20)

boxplot(NumSpecies ~ factor(type), data = subset(ucs, NumSpecies > 0 & type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), log="y", xlab = "Tipo de UC", ylab = "Número de espécies", main = "Número de espécies por tipo de UC")
savePlot("plots/numSpecies_log.png")
boxplot(NumSpecies ~ factor(type), data = subset(ucs, type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), xlab = "Tipo de UC", ylab = "Número de espécies", main = "Número de espécies por tipo de UC")
savePlot("plots/NumSpecies.png")

plot(NumSpecies ~ area, data = subset(ucs, NumRecords > 0))
plot(NumSpecies ~ NumRecords, data = subset(ucs, NumRecords > 0), xlab = "Número de registros", ylab = "Número de espécies", main = "Relação entre número de registros e de espécies")
savePlot("plots/registros_especies.png")

# Number of high quality taxons in each list
confLoc <- make_summary(dtTreated, "confidenceLocality", levels=c("High", "Medium", "Low", "None"), UC=Nome.da.UC)
ucs <- merge(ucs, confLoc, all=T)
confTax <- make_summary(dtTreated, "tax.check", levels=c("high", "medium", "low", "unknown"), UC=Nome.da.UC)
ucs <- merge(ucs, confTax, all=T)
originLoc <- make_summary(dtTreated, "selectionCategory", levels=c("coords_original", "coords_gazet", "coords_both", "locality_exact", "intersect_high", "intersect_medium", "plantr_exact"), UC=Nome.da.UC)
ucs <- merge(ucs, originLoc, all=T)

summary(ucs[,c("High", "Medium", "Low", "None")]/ucs$NumRecords)
summary(ucs[,c("high", "medium", "low", "unknown")]/ucs$NumRecords)
summary(ucs[,c("coords_original", "coords_gazet", "coords_both", "locality_exact", "intersect_high", "intersect_medium", "plantr_exact")]/ucs$NumRecords)

confLoc <- make_summary(dtOrig, "confidenceLocality", levels=c("High", "Medium", "Low", "None"), labels = c("HighF", "MediumF", "LowF", "NoneF"), UC=Nome.da.UC)
ucs <- merge(ucs, confLoc, all=T)
confTax <- make_summary(dtOrig, "tax.check", levels=c("high", "medium", "low", "unknown"), labels = c("Ouro", "Prata", "Bronze", "Latão"), UC=Nome.da.UC)
ucs <- merge(ucs, confTax, all=T)
originLoc <- make_summary(dtOrig, "selectionCategory", levels=c("coords_original", "coords_gazet", "coords_both", "locality_exact", "intersect_high", "intersect_medium", "plantr_exact"),  labels=c("coords_originalF", "coords_gazetF", "coords_bothF", "locality_exactF", "intersect_highF", "intersect_mediumF", "plantr_exactF"), UC=Nome.da.UC)
ucs <- merge(ucs, originLoc, all=T)

TamanhoLista <- sapply(dtOrig, nrow)
ucs <- merge(ucs, data.frame(Nome.da.UC, TamanhoLista), all=T)

summary(ucs[,c("HighF", "MediumF", "LowF", "NoneF")]/ucs$TamanhoLista)
summary(ucs[ucs$NumRecords>100,c("HighF", "MediumF", "LowF", "NoneF")]/ucs$TamanhoLista[ucs$NumRecords>100])
summary(ucs[ucs$NumRecords>1000,c("HighF", "MediumF", "LowF", "NoneF")]/ucs$TamanhoLista[ucs$NumRecords>1000])
ucs$propGold <- ucs$Ouro/ucs$TamanhoLista
summary(ucs$propGold[ucs$NumRecords>10])
ucs$cat_size <- factor(ifelse(ucs$NumRecords>1000, ">1000", ifelse(ucs$NumRecords>100, "100-1000", "<100")), levels=c("<100", "100-1000", ">1000"), ordered=T)
boxplot(ucs$propGold~ucs$cat_size, main="Proporção de táxons com grau Ouro", xlab="Número de registros totais", ylab="Proporção de táxons com grau Ouro de confiança na identificação")
savePlot("propGoldtax.png")

(ucs[c(126,145),c("HighF", "MediumF", "LowF", "NoneF")]/ucs$TamanhoLista[c(126,145)])
x <- (colSums(ucs[,c("Ouro", "Prata", "Bronze", "Latão")], na.rm = T))
x/sum(x)
summary(ucs[ucs$NumRecords>0,c("Ouro", "Prata", "Bronze", "Latão")])
summary(ucs[,c("Ouro", "Prata", "Bronze", "Latão")]/ucs$TamanhoLista)
summary(ucs[ucs$NumRecords>1000,c("Ouro", "Prata", "Bronze", "Latão")]/ucs$TamanhoLista[ucs$NumRecords>1000])
(ucs[c(126,145),c("Ouro", "Prata", "Bronze", "Latão")]/ucs$TamanhoLista[c(126,145)])
summary(ucs[,c("coords_originalF", "coords_gazetF", "coords_bothF", "locality_exactF", "intersect_highF", "intersect_mediumF", "plantr_exactF")]/ucs$TamanhoLista)

# Draw map
# Get shape for São Paulo
geobr::list_geobr()
sp <- geobr::read_state("SP", year=2020)
plot(sf::st_geometry(sp))
savePlot("plots/sp.png")
uc_shapes <- merge(shapes, ucs)
plot(uc_shapes[,"NumRecords"], main = "Número de registros por UC")
savePlot("plots/numRegistros_mapa.png")
plot(uc_shapes[,"NumOuro"], main = "Número de registros Ouro por UC")
savePlot("plots/numRegistrosOuro_mapa.png")

length(dtOrig)
# proportion of entries listed in catalogoUCsBR
catalogo <- make_summary(dtCat, column="Já.listada", levels=c("Sim", "Não"))
tem_lista <- subset(catalogo, Sim>0)
head(tem_lista)

# proportion of gps vs text entries
selCats <- lapply(dtOrig, function(x) {
    x <- x$selectionCategory
    x <- factor(x)
    summary(x)
})
selCats <- dplyr::bind_rows(selCats)
selCats <- cbind(Nome.da.UC,selCats)
summary(selCats)

m <- merge(summ_ml, confLoc, all=T)
summary(m)
m[is.na(m)] <- 0
write.csv(m, file.path(results_dir, "summary_multilist.csv"), row.names=F)

selCats$total <- rowSums(selCats)
props <- 100*selCats/selCats$total

summary(props)

summary(subset(props, selCats$total > 10))
summary(subset(selCats, total > 10))

colSums(selCats)
100*colSums(selCats)/sum(selCats$total)

original[which(props$intersect_high==max(props$intersect_high))]
original[which(props$intersect_medium==max(props$intersect_medium))]


    # Add info about being new to catalogo
    UC_catalogo <- subset(catalogoCompleto, grepl(Nome_UC, Unidade.Conservação, perl = T, ignore.case = T))
    speciesCatalogo <- unique(UC_catalogo$scientificNameFull)
    listed <- finalList$Táxon_completo %in% UC_catalogo$Táxon
    finalList[,"Já listada"] <- ifelse(listed, "Sim", "Não")


# Proportion of each taxon rank
prop.gps <- lapply(dtOrig, function(x) {
    data.frame(High=sum(x$confidenceLocality=="High"),
    Low=sum(x$confidenceLocality=="Low"))
})


total <- dplyr::bind_rows(dtTreated)

t <- plantR::validateTax(total, generalist = T)

summary(sapply(total, is.na))
table(total$downloadedFrom, useNA="always")

table(is.na(total$id))
table(total$tax.notes)

total <- unique(total)
sp <- split(total, total$tax.notes)
sapply(sp, nrow)

x <- sp[[1]]
x <- getTaxonId(x)
table(x$taxon.rank)
table(is.na(x$id))
table(x$id)
table(x$family)
table(x$scientificNameAuthorship)
table(x$family.new)

total <- getTaxonId(total)

names(sp)

1802/22494

missingID <- subset(total, is.na(id))

sort(table(missingID$family.new, useNA="always")) # 27 NA
sort(table(missingID$genus.new, useNA="always")) # 119 NA??
sort(table(missingID$species.new, useNA="always")) # 389 NA??
sort(table(missingID$scientificName.new, useNA="always")) # 389 NA??

library(plantR)

retaxed <- prepSpecies(missingID, db="fbo")
str(retaxed)

table(is.na(retaxed$id))



(76+575)/3000
