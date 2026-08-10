
remove_spaces <- function(x) {
  x<- gsub(" +$","",x, perl=T)
  x<- gsub("^ +","",x, perl=T)
  x<- gsub("  +"," ",x, perl=T)
  x<- gsub(" +\\.",".",x, perl=T)
  x
}

remove_punct <- function(x) {
  x<- gsub("\\(.*\\)","",x, perl=T)
  x<- gsub("\\[.*\\]","",x, perl=T)
  x<- gsub(",|\\?|\\.|\\/|\\[|\\]|\\(|\\)|&"," ",x, perl=T)
  x
}

fix_sp <- function(x) {
    x <- gsub("^s(.?.?o?| #227;o) paulo", "sao paulo", x, ignore.case=T)
    gsub("(\\s|,|\\.|-)s(.?.?o?| #227;o) paulo", "\\1sao paulo", x, ignore.case=T)
}

#' Finalize formatting location
#'
#' This corresponds to the final part of plantR::formatLoc(), which is used in fixLocation()
#' @importFrom plantR strLoc prepLoc getLoc
finLoc <- function(x, ...) {
  print(table(x$resolution.gazetteer))
  # strLoc
  locs <- strLoc(x)
  locs$loc.string <- prepLoc(locs$loc.string) # priority string
  if ("loc.string1" %in% names(locs))
    locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string 1
  if ("loc.string2" %in% names(locs))
    locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string 2

  # getLoc
  locs <- getLoc(x = locs, ...)
  colunas <- c("loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
  colunas <- colunas[colunas %in% names(locs)]
  x[,colunas] <- NULL
  x <- cbind.data.frame(x,
                         locs[, colunas], stringsAsFactors = FALSE)
  x[x==""] <- NA
  print(table(x$resolution.gazetteer))
  x
}
