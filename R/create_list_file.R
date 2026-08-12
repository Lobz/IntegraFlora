
top_records <- function(x, n = 5) {
    # Get best specimen/identification combo
    x <- x[order(x$ConfiançaLoc, x$ConfiançaID, !is.na(x$Imagens), as.numeric(x$AnoColeta), as.numeric(x$AnoID), na.last=F, decreasing = T),]
    # Split data into best record and not-best
    dup <- duplicated(x$Táxon_completo)
    res <- list(
        top = x[!dup,],
        extra = x[dup,]
    )
    # Select up to five non-best extra records
    sp <- split(res$extra, res$extra$Táxon_completo)
    sp <- lapply(sp, function(y) {
        if(nrow(y) > n) return(y[1:n,])
        else return(y)})
    res$extra <- do.call(rbind, sp)
    # return
    res
}

format_list <- function(x, UC) {
    finalList <- data.frame(
        UC = UC,
        Grupos = x$group,
        Família = x$family.new,
        Gênero = x$genus.new,
        Espécie =  sub("^.+ ","",x$species.new),
        Autor = x$scientificNameAuthorship.new,
        Táxon_completo = paste(toupper(x$family.new), x$scientificNameFull),
        Barcode = getBarcode(x),
        BD_Origem = x$downloadedFrom,
        Herbário = x$collectionCode.new,
        Coletor = x$recordedBy.new,
        Número_da_Coleta = x$recordNumber,
        Origem_FFBr = x$origin,
        AnoColeta = x$year.new,
        Identificador = x$identifiedBy.new,
        AnoID = x$yearIdentified.new,
        ConfiançaID = factor(x$tax.check, levels=c("unknown", "low", "medium", "high"), labels=c("Latão", "Bronze", "Prata", "Ouro")),
        ConfiançaLoc = factor(x$confidenceLocality, levels=c("None", "Low", "Medium", "High"), labels=c("Latão", "Bronze", "Prata", "Ouro")),
        Imagens = x$associatedMedia,
        Município = x$municipality.correct,
        Localidade = x$locality
    )
    finalList[order(finalList$Táxon_completo),]
}

#' Return a checklist file to a DWC format
#'
#' @param x A data.frame
#' @return A data.frame formatted with formatDwc
reverseListFormatting <- function(x) {
    x <- data.frame(
        family = x$Família,
        genus = x$Gênero,
        species = paste(x$Gênero, x$Espécie),
        scientificName = sub("^[A-Z]+ ", "", x$Táxon_completo),
        scientificNameAuthorship = x$Autor,
        barcode = x$Barcode,
        collectionCode = x$Herbário,
        recordedBy = x$Coletor,
        recordNumber = x$Número_da_Coleta,
        year = x$AnoColeta,
        origin = x$Origem_FFBr,
        identifiedBy = x$Identificador,
        yearIdentified = x$AnoID,
        associatedMedia = x$Imagens,
        downloadedFrom = x$BD_Origem,
        municipality = x$Município,
        locality = x$Localidade
    )

    x <- removeRepeatedAuthorship(x)

    x
}


getBarcode <- function(x) {
    ifelse(!is.na(x$barcode), x$barcode,
    ifelse(grepl("[A-Z]+", x$catalogNumber), x$catalogNumber,
    ifelse(is.na(x$associatedMedia), x$catalogNumber,
            str_extract(x$associatedMedia, "[A-z]+[0-9]+"))))
}
