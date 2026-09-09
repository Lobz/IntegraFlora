#' Functions to clean results folders
clean_folder <- function(folder, ...) {
    files <- list.files(folder, full.names = TRUE, recursive = TRUE)
    sapply(files, file.remove)
}

clean_total <- function(results_dir = Sys.getenv("RESULTS_DIR")) {
    if(results_dir == "") results_dir <- "results"
    clean_folder(file.path(results_dir, "total"), pattern = "*.rda")
}

clean_lists <- function(results_dir = Sys.getenv("RESULTS_DIR")) {
    if(results_dir == "") results_dir <- "results"
    clean_folder(file.path(results_dir, "total-treated"), pattern = "*.csv")
    clean_folder(file.path(results_dir, "checklist"), pattern = "*.csv")
}
