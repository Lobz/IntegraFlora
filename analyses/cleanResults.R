results_dir <- Sys.getenv("RESULTS_DIR")
raw <- list.files(file.path(results_dir, "total"), pattern = "*.rda", full.names = TRUE, recursive = TRUE)
tt <- list.files(file.path(results_dir, "total-treated"), pattern = "*.csv", full.names = TRUE, recursive = TRUE)
checklist <- list.files(file.path(results_dir, "checklist"), pattern = "*.csv", full.names = TRUE, recursive = TRUE)

sapply(c(raw,tt,checklist), file.remove)
