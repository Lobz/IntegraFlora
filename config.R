# Change the values in this file to fit your needs

# Data will be treated in chunks of chunk_size rows. Reduce this number if your RAM is limited
# Default is 4e5 (400000)
chunk_size = 2e5
# Options for parallelization in treating data
PARALLEL = TRUE
CORES = max(2, parallel::detectCores()-2)

# This tool is optimized to treat a single province at a time
COUNTRY = "Brazil"
STATEPROVINCE = "São Paulo"
