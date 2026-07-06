if(!require(integraFlora)) devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)

tt <- list.files("results/total", full.names = T)
dt <- lapply(tt, function(s) {load(s); return(total)})
nome_file <- sub(".*/","",tt)
nome_file <- sub(".csv","",nome_file)
names(dt) <- nome_file

ucs <- read.csv("results/summary_multilist.csv")
ucs$nome_file <- gsub(" ","",tolower(rmLatin(ucs$Nome.da.UC)))
rownames(ucs) <- ucs$nome_file

total <- dplyr::bind_rows(dt)
for(x in nome_file){
    dt[[x]]$Nome_UC <- ucs[x,1]
}

locTable <- function(x) {
    if(nrow(x)==0) {
        return(NULL)
    }
    x <- subset(x, !confidenceLocality %in% c("High", "Medium"))
    if(nrow(x)==0) {
        return(NULL)
    }
    x$locality <- gsub("Sr\\. $","Sr ",x$locality)

    locsList <- stringr::str_split(x$locality,",|[.]|;| - ")
    lens <- sapply(locsList, length)
    id <- sapply(1:nrow(x), function(i) rep(x$recordID[i],lens[i]))
    muns <- sapply(1:nrow(x), function(i) rep(x$municipality.correct[i],lens[i]))
    states  <- sapply(1:nrow(x), function(i) rep(x$stateProvince.correct[i],lens[i]))
    locs <- unlist(locsList)
    id <- unlist(id)
    muns <- unlist(muns)
    states <- unlist(states)
    length(muns)==length(locs)

    locs <- sub("\\.$","",locs)
    locs <- plantR:::squish(locs)
    Local <- c(x$locality.new, x$locality, x$locality.scrap, x$locality.correct)
    ID <- rep(x$recordID, 4)
    Muns <- rep(x$municipality.correct, 4)
    States <- rep(x$stateProvince.correct, 4)
    Local <- sub("\\.$","",Local)
    Localidade <- c(locs, Local)
    recordID <- c(id, ID)
    Municipio <- c(muns, Muns)
    Estado <- c(states, States)
    DT <- data.frame(recordID, Estado, Municipio, Localidade)
    if(nrow(DT)==0) {
        return(NULL)
    }
    DT <- subset(DT, nchar(Localidade) > 2 & !tolower(rmLatin(Localidade)) %in% c("sao paulo", "brasil", "brazil", "faz", "floresta ombrofila densa", "mata secundaria") & grepl("[A-z]",Localidade))
    if(nrow(DT)==0) {
        return(NULL)
    }

    DT$Municipio[is.na(DT$Municipio)] <- "" # So that aggregate doesn't exclude these cases
    DT$Estado[is.na(DT$Estado)] <- "" # So that aggregate doesn't exclude these cases

    LT <- aggregate(DT$recordID, list(Localidade = DT$Localidade, Municipio = DT$Municipio, Estado = DT$Estado), function(y) length(unique(y)))

    names(LT)[4] <- "Freq"
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Localidade, decreasing = T),]
    LT <- LT[!duplicated(tolower(rmLatin(paste(LT$Municipio, LT$Localidade)))),]

    LT$Nome_UC <- x$Nome_UC[1]
    LT[,c(5,1:4)]
}


locTable2 <- function(x) {
    if(nrow(x)==0) {
        return(NULL)
    }
    x <- subset(x, !confidenceLocality %in% c("High", "Medium"))
    if(nrow(x)==0) {
        return(NULL)
    }

    DT <- x[,c("recordID", "locality.new", "municipality.correct","stateProvince.correct")]
    DT2 <- DT
    DT2$locality.new <- x$locality.scrap
    DT <- rbind(DT, DT2)
    names(DT) <- c("recordID", "Localidade", "Municipio", "Estado")

    DT <- subset(DT, nchar(Localidade) > 2 & !tolower(rmLatin(Localidade)) %in% c("sao paulo", "brasil", "brazil", "faz", "floresta ombrofila densa", "mata secundaria") & grepl("[A-z]",Localidade))
    if(nrow(DT)==0) {
        return(NULL)
    }

    DT$Municipio[is.na(DT$Municipio)] <- "" # So that aggregate doesn't exclude these cases
    DT$Estado[is.na(DT$Estado)] <- "" # So that aggregate doesn't exclude these cases

    LT <- aggregate(DT$recordID, list(Localidade = DT$Localidade, Municipio = DT$Municipio, Estado = DT$Estado), function(y) length(unique(y)))

    names(LT)[4] <- "Freq"
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Localidade, decreasing = T),]
    LT <- LT[!duplicated(tolower(rmLatin(paste(LT$Municipio, LT$Localidade)))),]

    LT$Nome_UC <- x$Nome_UC[1]
    LT[,c(5,1:4)]
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

locTable3 <- function(df, filter=""){

    # df <- validateDup(df)
    # Formating the locality information
    occs.fix <- fixLoc(df)
    # Creating locality strings used to query the gazetteer
    occs.locs <- strLoc(occs.fix)
    occs.locs1 <- occs.locs
    # Final editing the locality strings (reduces variation in locality notation)
    occs.locs$loc.string <- prepLoc(occs.locs$loc.string)
    occs.locs$loc.string1 <- prepLoc(occs.locs$loc.string1)
    occs.locs$loc.string2 <- prepLoc(occs.locs$loc.string2)

    if(filter != "") {
        occs.locs$loc.string[grepl(filter, occs.locs$loc.string)] <- NA
        occs.locs$loc.string1[grepl(filter, occs.locs$loc.string1)] <- NA
        occs.locs$loc.string2[grepl(filter, occs.locs$loc.string2)] <- NA
    }

    locs.basic <- getLoc(occs.locs)
    locs.basic$loc.orig <- NA
    locs.subs <- tryAgain(locs.basic, function(x) x$resolution.gazetteer!="locality", add_cols = T, FUN =function(occs) {
        occs <- remove_fields(occs, c("loc", "loc.correct", "resolution.gazetteer", "latitude.gazetter", "longitude.gazetteer"))
        occs$loc.orig <- occs$loc.string1
        occs$loc.string1 <- gsub("\\s*,.*","",occs$loc.string1)
        occs <- getLoc(occs)
        occs
    })
    locs.subs <- tryAgain(locs.subs, function(x) x$resolution.gazetteer!="locality", add_cols = T, FUN =function(occs) {
        occs <- remove_fields(occs, c("loc", "loc.correct", "resolution.gazetteer", "latitude.gazetter", "longitude.gazetteer"))
        occs$loc.orig <- occs$loc.string2
        occs$loc.string2 <- gsub("\\s*,.*","",occs$loc.string2)
        occs <- getLoc(occs)
        occs
    })

    adm <- getAdmin(locs.subs)

    locs.subs$stateProvince <- ifelse(is.na(adm$NAME_1), locs.subs$stateProvince.new, adm$NAME_1)
    locs.subs$municipality <- ifelse(is.na(adm$NAME_2), plantR:::squish(rmLatin(tolower(df$municipality))), adm$NAME_2)
    locs.subs$locality <- plantR:::squish(rmLatin(tolower(df$locality)))

    locs.subs$lat <- ifelse(df$origin.coord=="coords_original",df$decimalLatitude.new,NA)
    locs.subs$lon <- ifelse(df$origin.coord=="coords_original",df$decimalLongitude.new,NA)
    locs.subs$source <- df$downloadedFrom

    locs.fixed <- subset(locs.subs, !is.na(loc.orig))
    locs.unfix <- subset(locs.subs, is.na(loc.orig))
    my_locs <- c(locs.unfix$loc.string, locs.unfix$loc.string1, locs.unfix$loc.string2)
    locs <- rbind(locs.unfix, locs.unfix, locs.unfix)
    locs$loc.orig <- my_locs

    locs <- subset(locs, loc.orig != loc.correct)
    locs$loc.correct <- ""
    locs[is.na(locs)] <- ""
    DT <- rbind(locs.fixed, locs)

    comb.text <- function(x) {
        x <- gsub("[,.]","",x)
        if(length(unique(x))==1) return(x[1])
        t <- sort(table(x), decreasing = T)
        ta <- paste(names(t),"(",t,")")
        paste(head(ta[ta>0]), collapse=" | ")
    }

    #source (and loc and loc.correct)
    LT <- aggregate(DT$source, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), function(x) paste(sort(unique(tolower(x))),collapse="_"))
    names(LT)[3] <- "source"
    #status in gazet and frequency here
    LT_tmp <- aggregate(DT$loc.orig, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), length)
    LT$Freq <-LT_tmp$x
    gazetteer <- read.csv("data-tmp/gazetteer - gazetteer_new.csv")

    locs_existentes <- unique(gazetteer$loc)

    LT$alreadyInGazet <- LT$loc %in% locs_existentes
    match_gazet <- match(LT$loc, gazetteer$loc) # todo: check which line is used in case of dups
    LT$ordemGazet <- gazetteer$order[match_gazet]
    LT$statusInGazet <- gazetteer$status[match_gazet]
    #country
    LT$country <- "Brazil"
    #state
    LT_st <- aggregate(DT$stateProvince, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), function(x) paste(unique(x), collapse="|"))
    LT$stateProvince <- LT_st$x
    #mun
    LT_mun <- aggregate(DT$municipality, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), comb.text)
    LT$municipality <- LT_mun$x
    #locality
    LT_locality <- aggregate(DT$locality, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), comb.text)
    LT$locality <- LT_locality$x
    #lat
    LT_tmp <- aggregate(as.numeric(DT$lat), list(loc = DT$loc.orig, loc.correct = DT$loc.correct), mean, na.rm=T)
    LT$meanLatitude <-LT_tmp$x
    #lon
    LT_tmp <- aggregate(as.numeric(DT$lon), list(loc = DT$loc.orig, loc.correct = DT$loc.correct), mean, na.rm=T)
    LT$meanLongitude <-LT_tmp$x

    LT_final <- LT[order(LT$Freq, decreasing = T),]

    LT_final
}

df <- subset(total, confidenceLocality == "Low" & resolution.gazetteer != "locality")
LT_final <- locTable3(df)

names(gazetteer)

upl <- read.csv("data-tmp/locs frequentes - locationsTable_getLoc_new.csv")

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

load("data-tmp/reflora_gbif_jabot_splink_saopaulo.RData")


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


old <- read.csv("data-tmp/temp_unused locations.csv")

removed <- old$loc
LT_final <- subset(LT_final, !loc %in% removed)

LT <- dplyr::bind_rows(old[1,],LT_final)[-1,]

write.csv(LT, "data-input/Locations/extraTables/unused_locs.csv", na="", row.names = F)
