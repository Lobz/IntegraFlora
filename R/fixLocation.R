
#' Fix Location
#'
#' A more elaborate version of plantR::formatLoc, focused on fixing locations within Brazil, especially in São Paulo.
#'
#' @param dt A data.frame treated with plantR::formatDwc() and plantR::formatOcc()
#' @param selectedCountry The name of a country, usually "Brazil".
#'
#' @importFrom utils read.csv
#' @importFrom plantR formatLoc
#' @export
fixLocation <- function(dt, selectedCountry = "Brazil") {

    print(paste("Found", nrow(dt), "records."))

    # get municipalities with unique name
    print("Loading municipality gazetteer...")
    munis <- read.csv("data-input/Locations/extraTables/municipalityGazetteer.csv")
    gazet = rbind(plantR:::gazetteer, munis)

    # load complementary gazetteer
    print("Loading extra gazetteer...")
    extra_gazet <- read.csv("data-input/Locations/extraTables/locGazetteer.csv")
    extra_gazet <- subset(extra_gazet, (!loc %in% gazet$loc) & (loc.correct %in% gazet$loc.correct), select = c("loc", "loc.correct"))
    extra_gazet_filled <- merge(extra_gazet, gazet[!duplicated(gazet$loc.correct),c(1,3:6)], by="loc.correct", all=F)[,names(gazet)]
    str(extra_gazet_filled)

    gazet <- rbind(gazet, extra_gazet_filled)

    print("Formatting loc...")
    if(!"locality" %in% names(dt)) dt$locality <- NA
    if(!"municipality" %in% names(dt)) dt$municipality <- dt$county
    dt$municipality <- sub("([ ,\\.^])sta\\.","\\1santa", dt$municipality, ignore.case = T)
    dt$locality <- sub("([ ,\\.^])sta\\.","\\1santa", dt$locality, ignore.case = T)

    dt$country <- remove_spaces(dt$country)
    dt$stateProvince <- remove_spaces(dt$stateProvince)
    dt$municipality <- remove_spaces(dt$municipality)
    dt$locality <- remove_spaces(dt$locality)
    dt <- formatLoc(dt, gazet = gazet)

    # gonna hand redo formatLoc
    # fixLoc is already done, thank you
    dt$stateProvince.new <- fix_sp(dt$stateProvince.new)
    dt$municipality.new <- fix_sp(dt$municipality.new)
    dt$locality.new <- fix_sp(dt$locality.new)

    dt$country.new <- remove_spaces(dt$country.new)
    dt$stateProvince.new <- remove_spaces(dt$stateProvince.new)
    dt$municipality.new <- remove_spaces(dt$municipality.new)
    dt$locality.new <- remove_spaces(dt$locality.new)

    print("Fixing country name...")
    dt <- tryAgain(dt, function(x) {
        x$resolution.gazetteer %in% c("no_info") &
        grepl("mog. mirim|campinas|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|cardoso|botucatu|moj. mirim|sao paulo",x$municipality.new)
    }, function(x) {

        x$country.new <- "brazil"
        x <- finLoc(x, gazet = gazet)
    })

    dt <- tryAgain(dt, function(x) {
        x$resolution.gazetteer %in% c("no_info") &
        grepl("mog. mirim|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|botucatu|moj. mirim|sao paulo",x$locality.new)
    }, function(x) {
        x$country.new <- "brazil"
        x <- finLoc(x)
    })

    dt <- tryAgain(dt, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("bra[sz]il",x$country.new), function(x) {

    x$country.new <- "brazil"
    x <- finLoc(x)
    })

    dt <- tryAgain(dt, function(x) {
        x$resolution.gazetteer %in% c("no_info") &
        x$stateProvince.new %in% c("sp","sao paulo","ceara","pernambuco","minas gerais")
    }, function(x) {
        x$country.new <- "brazil"
        x <- finLoc(x)
    })

    print("Fixing state name...")
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "country",finLoc,  gazet = gazet)

    # fix state name
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "country", function(x) {
    x$locality.new[grepl("arquipelago (de ?)sao pedro e sao paulo",x$stateProvince.new)] <- "arquipelago de sao pedro e sao paulo"
    x$stateProvince[grepl("arquipelago (de ?)sao pedro e sao paulo",x$stateProvince.new)] <- "pernambuco"
    x$stateProvince.new<- remove_punct(x$stateProvince.new)
    x$stateProvince.new<- gsub("state","",x$stateProvince.new, fixed = TRUE)
    x$stateProvince.new<- gsub("estado","",x$stateProvince.new, fixed = TRUE)
    x$stateProvince.new<- gsub("est +(d. )?","",x$stateProvince.new)
    x$stateProvince.new<- gsub("of|d. ","",x$stateProvince.new)
    x$stateProvince.new<- gsub("  +"," ",x$stateProvince.new)
    x$stateProvince.new<- fix_sp(x$stateProvince.new)
    x$stateProvince.new<- gsub("sao paulo sp?","sao paulo",x$stateProvince.new)
    x$stateProvince.new<- remove_spaces(x$stateProvince.new)
    if(selectedCountry == "Brazil") {
        x$stateProvince.new<- gsub("san pablo","sao paulo",x$stateProvince.new, fixed = TRUE)
        x$stateProvince.new<- gsub("sao paolo","sao paulo",x$stateProvince.new, fixed = TRUE)
        x$stateProvince.new[grepl("sao paulo",x$stateProvince.new, fixed = TRUE)] <- "sao paulo"
    }
    x$stateProvince.new[grepl("s #227;o paulo",x$stateProvince.new, fixed = TRUE)] <- "sao paulo"

    # x$municipality.new <- NA

    x <- finLoc(x, gazet = gazet)

    })

    # Get those MEX002 cases
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer %in% c("country") & grepl("&SAO PAULO", x$locality, fixed=T), function(x) {
    x$municipality.new <- tolower(sub("&SAO PAULO","",x$locality))
    x$stateProvince.new <- "sao paulo"
    x$locality.new <- x$municipality.new

    x <- finLoc(x)
    })

    dt <- tryAgain(dt, function(x) x$resolution.gazetteer %in% c("country", "no_info"), function(x) {
        x$municipality.new[grepl("ubatuba",x$stateProvince.new)] <- "ubatuba"
        x$locality.new[grepl("ubatuba",x$stateProvince.new)] <- "ilha anchieta"
        towns <- grepl("mog. mirim|campinas|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|cardoso|botucatu|moj. mirim",x$stateProvince.new) & is.na(x$municipality.new)
        x$municipality.new[towns] <- x$stateProvince[towns]
        x$stateProvince.new[towns] <- "sao paulo"
        x$country.new[towns] <- "brazil"
        x$stateProvince.new[x$stateProvince.new=="sp"] <- "sao paulo"

        x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
        stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
        x$municipality.new <- gsub(stateStr,"",x$municipality.new)
        x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
        x$municipality.new<- remove_punct(x$municipality.new)
        x$municipality.new<- remove_spaces(x$municipality.new)

        x <- finLoc(x, gazet = gazet)
    })

    munis <- read.csv("data-input/Locations/extraTables/uniqueMunicipalities.csv")
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality.new), function(x) {
        # find state name in municipality name
        x$stateProvince.new <- munis$stateProvince.new[match(x$municipality.new, munis$municipality.new)]

        x <- finLoc(x, gazet = gazet)
    })

    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "country" & is.na(x$municipality), function(x) {
        # find state name in state name
        muni <- tolower(rmLatin(x$stateProvince))
        x$stateProvince.new <- munis$stateProvince.new[match(muni, munis$municipality.new)]
        x$municipality.new <- muni

        x <- finLoc(x, gazet = gazet)
    })

    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality.new), function(x) {
        # find state name in municipality name
        x$municipality.new <- fix_sp(x$municipality.new)
        x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
        stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
        x$municipality.new <- gsub(stateStr,"",x$municipality.new)
        x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
        x$municipality.new<- remove_punct(x$municipality.new)
        x$municipality.new<- remove_spaces(x$municipality.new)
        x$stateProvince.new <- munis$name_state_norm[match(x$municipality.new, tolower(rmLatin(munis$name_muni)))]

        x <- finLoc(x, gazet = gazet)
    })

    # get admin names
    dt <- addAdmin(dt)

    print("Fixing municipality name...")
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "state" & !is.na(x$stateProvince.correct), function(x) {

        print(table(x$resolution.gazetteer))
        country <- x$country
        x$country <- x$country.correct
        state <- x$stateProvince
        x$stateProvince <- x$stateProvince.correct
        x <- formatLoc(x, gazet = gazet)
        # Return verbatim info to original
        x$stateProvince <- state
        x$country <- country
        print(table(x$resolution.gazetteer))
        x
    }, success_condition = function(x) x$resolution.gazetteer %in% c("county","locality"), label = "Using correct state name")

    print("Fixing locality name...")
    dt <- tryAgain(dt, function(x) x$resolution.gazetteer == "county" & !is.na(x$municipality.correct), function(x) {

        print(table(x$resolution.gazetteer))
        country <- x$country
        x$country <- x$country.correct
        state <- x$stateProvince
        x$stateProvince <- x$stateProvince.correct
        county <- x$municipality
        x$municipality <- x$municipality.correct
        x <- formatLoc(x, gazet = gazet)
        # Return verbatim info to original
        x$country <- country
        x$stateProvince <- state
        x$municipality <- county
        print(table(x$resolution.gazetteer))
        x
    }, success_condition = function(x) x$resolution.gazetteer %in% c("locality"), label = "Using correct state and municipality name")

    dt <- tryAgain(dt, function(x) x$loc == "brazil_NA_sao paulo" & !is.na(x$locality.new), finLoc, function(x) x$resolution.gazetteer=="locality")

    dt <- addAdmin(dt)

    return(dt)
}
