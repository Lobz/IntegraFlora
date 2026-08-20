#' @title Converts data from Reflora to DarwinCore
#'
#' @param data Data.table downloaded from Reflora
#'
#' @author Mali Oz Salles and Pablo Melo
#'
#' @importFrom dplyr mutate %>%
#'
#' @encoding UTF-8
#'
#' @keywords internal
#'
parseReflora <- function(data) {
    print(paste0('n. registros: ', nrow(data)))
    print(paste0('n. colunas: ', ncol(data)))

    # Replace names with DWC names
    english <- FALSE
    if(all(names_reflora$nome_reflora == names(data))) {
        names(data) <- names_reflora$nome_dwc
    } else if(all(names_reflora$nome_reflora_eng == names(data))) {
        names(data) <- names_reflora$nome_dwc
        english <- TRUE
    } else {
        stop("Os nomes das colunas em um arquivo Reflora não estão no padrão esperado. Verififique o padrão esperado em reflora_fields.csv")
    }

    if(english) {
        data$taxonRank[data$taxonRank=="Subfamily"] <- "Genus"
        data$taxonRank = factor(data$taxonRank,
            levels = c('Form','Variety','Subespecies','Species','Genus','Family','Order', 'Class', 'Philum', 'Kingdom'),
            labels = taxonRanks,
            ordered=TRUE)
    } else {
        data$taxonRank[data$taxonRank=="Subfamília"] <- "Gênero"
        data$taxonRank = factor(data$taxonRank,
            levels = c('Forma','Variedade','Subespécie','Espécie','Gênero','Família','Ordem', 'Classe', 'Filo', 'Reino'),
            labels = taxonRanks,
            ordered=TRUE)
    }

    data <- data %>% dplyr::mutate(
        # source = 'reflora',
        # comments = '',

        scientificName = substr(verbatimScientificName, nchar(family)+2, nchar(verbatimScientificName)),

        decimalLatitude = verbatimLatitude,
        decimalLongitude = verbatimLongitude,
        county = NA,
        institutionCode = collectionCode
    )

    return(data)
}
