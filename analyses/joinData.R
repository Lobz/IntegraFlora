if(!require(integraFlora)) devtools::load_all()
source("config.R")

print("Loading data...")

load(file.path(Sys.getenv("DATATMP"), "gbif.rda"))
load(file.path(Sys.getenv("DATATMP"), "reflora.rda"))
load(file.path(Sys.getenv("DATATMP"), "jabot.rda"))
load(file.path(Sys.getenv("DATATMP"), "splink.rda"))
load(file.path(Sys.getenv("DATATMP"), "other.rda"))

# Join in a single list
print("Joining...")
all_data <- c(gbif, reflora, jabot, splink, other)
rm(gbif, reflora, jabot, splink, other)
print(paste("Found", sum(vapply(all_data, nrow, 0)), "records in", length(all_data), "files"))

# Organize into bite-sized chunks (size from config? ~500k?)
if(!exists("chunk_size")) chunk_size<- 4e5

sizes <- vapply(all_data, nrow, 0)
# Remove empty sets
if(any(sizes == 0)) {
    warning("There are empty sets in data. Check for errors.")
    all_data <- all_data[sizes > 0]
    sizes <- sizes[sizes > 0]
}
if(any(sizes > chunk_size)) {
    print("Splitting large files into chunks...")
    small <- all_data[sizes <= chunk_size]
    all_data <- all_data[sizes > chunk_size]
    all_data <- lapply(all_data, function(x) {
        split(x, rep(1:ceiling(nrow(x)/chunk_size), each=chunk_size))
    })
    all_data <- c(small,do.call(c, all_data))
    small <- NULL
    sizes <- sapply(all_data, nrow)
}
if(any(sizes < chunk_size/2)) {
    print("Joining small files...")
    all_data <- all_data[order(sizes, decreasing = TRUE)]
    large <- 1
    small <- length(all_data)
    while(large != small) {
        print(paste("Large: ", large, " / Small: ", small))
        sum <- nrow(all_data[[small]]) + nrow(all_data[[large]])
        print(sum)
        if(sum <= chunk_size) {
            all_data[[large]] <- join(all_data[[large]], all_data[[small]], merge = FALSE)
            all_data[small] <- NULL
            small <- small - 1
        } else {
            large <- large + 1
        }
    }
}

print(paste("Organized", sum(sapply(all_data, nrow)), "records in", length(all_data), "chunks of", as.integer(chunk_size), "records"))
save(all_data, file=file.path(Sys.getenv("DATATMP"),"all_data.rda"))
