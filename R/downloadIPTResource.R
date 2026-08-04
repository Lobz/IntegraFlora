# Download JABOT data -----------------------------------------------------
# Author: guilherme gritz
# https://ipt.jbrj.gov.br/jabot/resource?r= is the base url.

#' Download IPT Resource
#'
#' @author Guilherme Gritz
#' @author Mali Oz Salles
#'
#' @param resource Name of the resource (ie, collection code)
#' @param baseUrl Start of the URL, lacking the resouce name
#' @param directory Destination folder
#' @param filename Destination file
#' @export
downloadIPTResource <- function(resource,
                                  baseUrl = "https://ipt.jbrj.gov.br/jabot/resource?r=",
                                  directory = here::here("data-input", "Occurrences", "OtherSources",
                                  "JABOT"),
                                  filename = paste0(resource, ".zip")) {
    if(!dir.exists(directory))
        dir.create(directory)

    cat("Downloading", resource, "\n")

    # Get JABOT IPT url from each herbarium
    url <- paste0(baseUrl, resource)

    # Get its html content
    html_content <- rvest::read_html(url, encoding = "ISO-8859-1")

    # Find the node containing the most updated version (SelectorGadget addon is useful here)
    node <- rvest::html_node(html_content, css = "td a")

    # Get href attribute
    link <- rvest::html_attr(node, "href")

    # Download and save
    fullname <- file.path(directory, filename)
    utils::download.file(url = link, destfile = fullname, mode = "wb")

    openZip(fullname, files = "occurrence.txt")
}


openZip <- function(file, ...) {
    utils::unzip(file, exdir = gsub(".zip", "", file), overwrite = T, ...)
}

downloadJabot <- function() {
    lapply(herbariaJabot, downloadIPTResource)
}

downloadReflora <- function() {
    lapply(herbariaReflora, downloadIPTResource,
        baseUrl = "https://ipt.jbrj.gov.br/reflora/resource?r=",
        directory = here::here("data-input", "Occurrences", "OtherSources",
        "Reflora"))
}
