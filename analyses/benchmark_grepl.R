
start <- "qywtruyqtwreuytdjhgvASNBVMNBVMNAdfsdfsdSJYGDJHGC"
end1 <- "ljhagsljhglajsfhgl"
end2 <- "jhgkjhgkjhgkjhgkjhgkjhg"

match1 <- paste0(start, end1)
match2 <- paste0(start, end2)

pattern1 <- paste0(match1, "|", match2)
pattern2 <- paste0(start, "(", end1, "|", end2, ")")

fun1 <- function(x) grepl(pattern1, x, perl = T)
fun2 <- function(x) grepl(pattern2, x, perl = T)
fun3 <- function(x) grepl(match1, x, fixed = T) | grepl(match2, x, fixed = T)

x <- replicate(100, paste0(sample(c(letters,LETTERS, start, end1, end2), 300, T), collapse=""))
y <- replicate(100, paste0(sample(c(letters,LETTERS, start, end1, end2), 300, T), collapse=""))

microbenchmark::microbenchmark(fun1(x), fun2(x), fun3(x))

fun4 <- function(x) grepl(paste0(match1, "|", match1), x, perl = T)
fun5 <- function(x) grepl(paste0(match1), x, perl = T)

microbenchmark::microbenchmark(fun4(x), fun5(x))

fun6 <- function(x,y) grepl(pattern1, x) | grepl(pattern1, y)
fun7 <- function(x,y) grepl(pattern1, paste(x,y))

microbenchmark::microbenchmark(fun6(x, y), fun7(x, y))

pattern3 <- paste0(start, "(?:", end1, "|", end2, ")")

fun8 <- function(x) grepl(pattern3, x, perl = T)

microbenchmark::microbenchmark(fun1(x), fun2(x), fun3(x), fun8(x))

devtools::load_all()

load("data-tmp/corpus.rda")


require(plantR)
source("config.R")

results_dir <- Sys.getenv("RESULTS_DIR")
if(results_dir == "") results_dir <- "results"

STATE <- Sys.getenv("STATE")
if(STATE == "") STATE <- STATEPROVINCE
STATEPROVINCE <- STATE

folder <- "data-input/Locations/info/"
info_files <- read.csv("data-input/Locations/info/file_descriptions.csv", na.strings = c("", "NA"))
filenames <- file.path(folder, info_files$arquivo)

# apply default values
info_files$sep[is.na(info_files$sep)] <- ","
info_files$dec[is.na(info_files$dec)] <- "."

# Read data according to file descriptions
info_data_list <- lapply(1:nrow(info_files), formatInfo)
# Subset to state and make names unique
info_data_list <- lapply(info_data_list, subset_uc_summary)

data <- do.call(rbind, info_data_list)
uc_names <- unique(data$name)

# Prepare for searching alternative names in the occurrence data
altNamesA <- consolidateNamesTable(checkedLocations = read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = uc_names, combineSearchStrings = combineSearchStrings)
altNamesB <- consolidateNamesTable(checkedLocations = read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = uc_names, combineSearchStrings = combine_uc_regex)
altNames0 <- consolidateNamesTable(checkedLocations = read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = uc_names, combineSearchStrings =
        function(x) paste0("(", paste(generate_uc_string(x), collapse = "|"), ")")
)

which(altNamesA$x != altNamesB$x)


funA <- function(x) lapply(x$x, function(p) searchLoc(p, corpus))

timeA <- system.time(resA <- funA(altNamesA))
timeB <- system.time(resB <- funA(altNamesB))
time0 <- system.time(res0 <- funA(altNames0))

timeA
timeB
time0

length(resA)
length(resB)
length(res0)

all(sapply(1:length(resA), function(i) all(resA[[i]] == resB[[i]])))
all(sapply(1:length(resA), function(i) all(resA[[i]] == res0[[i]])))
