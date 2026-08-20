
chunk <- function(all_data, chunk_size = 4e5, merge = T) {

    print(paste("Found", sum(vapply(all_data, nrow, 0)), "records in", length(all_data), "files"))

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
        large <- all_data[sizes > chunk_size]
        spl <- lapply(large, function(x) {
            split(x, rep(1:ceiling(nrow(x)/chunk_size), each=chunk_size))
        })
        all_data <- c(small,do.call(c, spl))
        sizes <- sapply(all_data, nrow)
    }
    if(any(sizes < chunk_size/2)) {
        print("Joining small files...")
        small <- all_data[sizes < chunk_size]
        large <- all_data[sizes >= chunk_size]
        small <- small[order(sapply(small, nrow))]
        while((l <- length(small))>1) {
            sum <- nrow(small[[1]]) + nrow(small[[l]])
            if(sum <= chunk_size) {
                small[[l]] <- join(small[[1]], small[[l]], merge)
                small <- small[2:l]
            } else {
                large <- c(large, small[l])
                small <- small[1:(l-1)]
            }
        }
        all_data <- c(small, large)
    }

    print(paste("Organized", sum(sapply(all_data, nrow)), "records in", length(all_data), "chunks of", as.integer(chunk_size), "records"))
    all_data
}