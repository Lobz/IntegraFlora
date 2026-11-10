if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

tmp_dir <- Sys.getenv("DATATMP")
if(tmp_dir == "") tmp_dir <- "data-tmp"

tt <- list.files(file.path(results_dir, "total"), full.names = T)
dt <- lapply(tt, function(s) {load(s); return(total)})
nome_file <- sub(".*/","",tt)
nome_file <- sub(".csv","",nome_file)
names(dt) <- nome_file

ucs <- read.csv(file.path(results_dir, "summary_multilist.csv"))
ucs$nome_file <- gsub(" ","",tolower(rmLatin(ucs$Nome.da.UC)))
rownames(ucs) <- ucs$nome_file

total <- dplyr::bind_rows(dt)
for(x in nome_file){
    dt[[x]]$Nome_UC <- ucs[x,1]
}

x <- locTable2(dt[[4]])
head(x)

for(x in dt) LT <- locTable(x)
tabs <- lapply(dt, locTable)
TABS <- dplyr::bind_rows(tabs)
# write.csv(TABS, "data-input/Locations/extraTablesTable.csv", row.names = F)
TABS2 <- read.csv("data-input/Locations/extraTables/locationsTable.csv")

TABS3 <- subset(TABS, Localidade %in% TABS2$Localidade)
write.csv(TABS3, "data-input/Locations/extraTables/locationsTable.csv", row.names = F)

df <- subset(total, confidenceLocality == "Low" & resolution.gazetteer != "locality")
LT_final <- locTable3(df)

names(gazetteer)

upl <- read.csv(file.path(file.path(tmp_dir,"locs frequentes - locationsTable_getLoc_new.csv"))

upl$Freq <- LT_final$Freq[match(upl$loc,LT_final$loc)]
head(upl)
upl$add <- ifelse(upl$alreadyInGazet..status=="No: add", "Yes", "")
# upl$status <- pairwiseMap(upl$Freq,upl$statusInGazet, max, na.rm=T)
upl <- upl[order(upl$Freq),]

write.csv(upl[upl$Freq>100,],"data-input/Locations/extraTables/locationsTable_getLoc.csv", row.names = F)

head(tabs_locs)
x <- subset(total, )

nrow(total)
table(total$selectionCategory, useNA="always")

loctab <- unique(total[,c("Nome_UC","locality")])

## Second source of tab: names that "look like" they might be UCs

load(file.path(file.path(tmp_dir,"corpus.rda"))


df <- subset(saopaulo, resolution.gazetteer != "locality")

loc_string <- paste(df$country, df$municipality, df$stateProvince, df$locality)
loc_string <- tolower(rmLatin(loc_string))

pat1 <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|hacienda|sitio|mata|horto|jardim|campus|pico|serra|sierra|morro|chapada|colina|monumento|exp|floresta|refugio|estacao|est.|a\\.p\\.a|faz\\.|faz |ecol\\.|biol\\.|p\\.e\\.|e\\.b\\.|cerrado|rvs"
terms_pattern <- tolower(rmLatin(paste(c(uc_abbrevs$short, uc_abbrevs$long, pat1), collapse="( |\\.|,|$)|")))

possible_ucs <- grepl(terms_pattern, loc_string)
tail(tab(loc_string[possible_ucs]))
df <- df[possible_ucs, ]
df <- validateDup(df)

LT_unused <- locTable3(df)

head(LT_unused)
write.csv(LT_unused, "data-input/Locations/extraTables/potential_locs_full.csv")
tail(LT_unused)
LT_final <- subset(LT_unused, Freq>10 & !alreadyInGazet)
head(LT_final)


old <- read.csv(file.path(tmp_dir, "temp_unused locations.csv"))

removed <- old$loc
LT_final <- subset(LT_final, !loc %in% removed)

LT <- dplyr::bind_rows(old[1,],LT_final)[-1,]

write.csv(LT, "data-input/Locations/extraTables/unused_locs.csv", na="", row.names = F)
