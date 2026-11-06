if(!require(integraFlora)) devtools::load_all()
library(plantR)

tmp_dir <- Sys.getenv("DATASTATE")
if(tmp_dir == "") tmp_dir <- "data-tmp"

print("Loading data...")
load(file.path(tmp_dir, "corpus-full.rda"))
try({
  nostate <- readRDS("treated-data/noStateInfo.rds")
  corpus <- dplyr::bind_rows(nostate, corpus)
})

badnames <- names(corpus)[endsWith(names(corpus), "1")]
if(length(badnames) > 0) {
  warning(paste("Removing problematic columns:", paste(badnames, collapse=", ")))
  corpus[,badnames] <- NULL
}

print("Removing duplicates...")
corpus <- my_valDup(corpus)

print("Saving...")
save(corpus, file=file.path(tmp_dir, "corpus.rda"))
