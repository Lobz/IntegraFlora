

plantRWorkflow_part1 <- function(x) {
    # Standardize missing information
    x[x==""] <- NA

    # # Subset country
    # print(dim(x))
    # x <- subset(x, is.na(country) | grepl("br", tolower(country), fixed=T))

    # Lets format this
    print("Formatting occs")
    x <- plantR::formatOcc(x, noNumb = NA, noYear = NA, noName = NA)

    print("Formatting locs")
    if(!exists("COUNTRY")) COUNTRY <- "Brazil"
    if(!exists("STATEPROVINCE")) STATEPROVINCE <- "São Paulo"
    x <- fixLocation(x)

    # Treat gps data
    print("Formatting coords...")
    x <- plantR::formatCoord(x)
    x
}

subsetToProvince <- function(x) {
    print("Subsetting to state...")

    tab(x$country.correct)
    tab(x$country.new[is.na(x$country.correct)])
    x <- subset(x, country.correct == COUNTRY)

    # noCountry <- subset(dt, is.na(country.correct))
    tab(x$stateProvince.correct)
    tab(x$municipality.new[is.na(x$stateProvince.correct)])
    x <- subset(x, stateProvince.correct == STATEPROVINCE | is.na(stateProvince.correct))

    # Select only records that have SOME location info
    noloc <- is.na(x$municipality) & is.na(x$locality) & (x$origin.coord == "coord_gazet")
    x <- x[!noloc,]
    x
}

plantRWorkflow_part2 <- function(x) {

    # formatTax and validateTax
    print("Formatting taxonomy...")
    x <- completeScientificName(x, rm.miss = T)
    x <- removeRepeatedAuthorship(x)

    data(list = c("bfoNamesBryophyta", "bfoNamesAlgae"), package = "plantRdata")
    tax <- dplyr::full_join(bfoNamesBryophyta, bfoNamesAlgae)
    x <- getTaxonId(x)
    # using the Bryophyta and Algae
    x <- tryAgain(x, not_found, getTaxonId, db = tax)

    x <- tryAgain(x, not_found, function(x) {
        x <- isolateAuthorship(x)
        x <- getTaxonId(x)
        # using the Bryophyta and Algae
        x <- tryAgain(x, not_found, getTaxonId, db = tax)
    })

    # We'll try getting extra taxons with wfo
    # loading the WFO and WCVP backbones into a temporary environment
    # using the World Flora Online
    # data(list = c("wfoNames", "wcvpNames"), package = "plantRdata")
    # x <- tryAgain(x, not_found, getTaxonId, db = wfoNames)
    # using the World Checklist of Vascular Plants
    # x <- tryAgain(x, not_found, getTaxonId, db = wcvpNames)

    # validate
    print("Validating location info...")
    x <- plantR::validateLoc(x)

    print("Validating identification info...")
    # validate taxonomist
    x <- plantR::validateTax(x, generalist = T)
    x$tax.check <- factor(x$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)


    print("Validating geolocation info...")
    map <- plantR::latamMap$brazil
    map <- subset(map, NAME_1 == "sao paulo")
    x <- plantR::validateCoord(x, high.map = map) # WORKING
    x <- tryAgain(x, function(x) is.na(x$decimalLatitude.new), plantR::formatCoord)
    x <- tryAgain(x, function(x) is.na(x$geo.check), plantR::validateCoord, high.map=map)
    tab(is.na(x$geo.check))
    table(x$geo.check, x$origin.coord)

    x
}
