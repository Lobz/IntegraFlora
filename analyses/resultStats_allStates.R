if(!require(integraFlora)) devtools::load_all()
library(geobr)
library(florabr)
# Load information for brazilian flora
if (length(list.files("data-tmp/florabr","*.rds", recursive = T))>0) {
    bf <- load_florabr(data_dir = "data-tmp/florabr")
} else {
    bf <- get_florabr(output_dir = "data-tmp/florabr")
}


results_folder <- "../ChecklistsBrazil"

# Get relevant dataset info
datasets <- list_geobr()

# Read states data from geobr
st_info <- datasets[datasets[,1]=="read_state",]
st_latest_year <- sub(".* ","",st_info$year)
states <- geobr::read_state(year = st_latest_year)$name_state

head(states)
states <- sort(states)

summ_gO<- lapply(states, function(f){
    t <- read.csv(file.path(results_folder, slug(f), "summary_getOccs.csv"))
    t$state <- f
    t$type <- factor(gsub("_.*","", slug(t$name)))
    t
})

# Número de UCs
sgO <- do.call(rbind, summ_gO)
nrow(sgO)
tab(sgO$state)
tab(sgO$type)

# Ucs com mais de 20 registros:
s0 <- subset(sgO, NumRecords > 0)
nrow(s0)
sum(tail(tab(s0$state),4))
tab(s0$type)

# Ucs com mais de 20 registros:
s20 <- subset(sgO, NumRecords > 20)
nrow(s20)
sum(tail(tab(s20$state),4))
tab(s20$type)
tab(s20$state)
sum(tail(tab(s20$state),8))

# Ucs com mais de 200 registros:
s200 <- subset(sgO, NumRecords > 200)
nrow(s200)
tab(s200$state)
tab(s200$type)
barplot(tab(s200$type), horiz=T)

# Ucs com mais de 200 registros:
s2000 <- subset(sgO, NumRecords > 2000)
nrow(s2000)
tab(s2000$state)
tab(s2000$type)
barplot(tab(s2000$type), horiz=T)




# Find candidate CSV files recursively under the project root
checklists <- list.files(
  path = results_folder,
  pattern = "modeloCatalogo\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

if (length(checklists) == 0) {
  message("No CSV files found inside any 'checklist' folder.")
} else {
  message("Found ", length(checklists), " CSV file(s) in checklist folders.")

  # Read all files into a list
  data_list <- lapply(checklists, function(f) {
    try(read.csv(f))
    })
    sapply(data_list, function(x) {
        sum(is.na(x$Espécie))
    })

    all_lists <- do.call(rbind, data_list)
    tops <- top_records(all_lists)

    top <- tops$top
    (num_species <- length(unique(paste(top$Gênero, top$Espécie))))

bf$Táxon_completo <- paste(toupper(bf$family), bf$scientificName)
bfgood <- bf[ bf$taxonomicStatus %in% "Accepted" & bf$origin %in% "Native" & bf$taxonRank %in% "Species" & bf$kingdom %in% "Plantae",]

(num_species_bf <- nrow(bfgood))

found <- bfgood$Táxon_completo %in% top$Táxon_completo
top2 <- merge(bf, top)
nrow(top2)

# Proporção de espécies encontradas
sum(found)/num_species_bf

tab(top2$Origem)
tab(top$UC)
tab(top$Família)

nat <- merge(bfgood, top)

natt <- tab(nat$family)
bft<- tab(bfgood$family)
head(bft)


bft <- (as.data.frame(bft))
head(bft)
names(bft) <- c("family", "total")
bft$family <- as.character(bft$family)
dt2 <- as.data.frame(natt)
names(dt2) <- c("family", "actual")
dt2$family <- as.character(dt2$family)

dt <- merge(bft,dt2, all.x=T)
nrow(dt)
dstt <- dt[order(dt$total),]
head(dt)
tail(dt)
dt[is.na(dt)] <- 0

dt$prop <- round(100*dt$actual/dt$total,1)
dt <- dt[order(dt$prop),]
dt

subset(dt, prop>30)
subset(dt, prop>70)
subset(dt, prop<30 & total>100)
subset(dt, total>20)


subset(top2, Família %in% "Brassicaceae")[,c("taxonRank","kingdom", "origin", "nomenclaturalStatus", "taxonomicStatus")]
subset(bfgood, family %in% "Brassicaceae")$Táxon_completo

  names(data_list) <- basename(checklists)

  # If you want one combined data frame, uncomment the next line.
  # combined_data <- do.call(rbind, data_list)

  # To inspect the list of loaded data frames:
  print(data_list)
}

# Make a smaller corpus output
ns <- data.frame(name1 = names(corpus), name2=names(corpus))
write.csv(ns, "data/names.csv", row.names=F)

rdsss <- lapply(states, function(f){
    load(file.path(results_folder, slug(f), "corpus.rda"))
    out <- formatRDS(corpus)

  saveRDS(out, file.path(results_folder, paste0(slug(f),".rds")))
})

    load(file.path(results_folder, slug(f), "corpus-full-NoSTATE.rda"))
    tab(corpus$id)
    saveRDS(corpus, "treated-data/noStateInfo.rds")
    out <- formatRDS(corpus)

  saveRDS(out, file.path(results_folder, "noStateInfo.rds"))
