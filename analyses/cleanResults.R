raw <- list.files("results/total", pattern = "*.rda", full.names = TRUE, recursive = TRUE)
tt <- list.files("results/total-treated", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
checklist <- list.files("results/checklist", pattern = "*.csv", full.names = TRUE, recursive = TRUE)

sapply(c(raw,tt,checklist), file.remove)
