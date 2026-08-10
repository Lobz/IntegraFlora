# Please read the README before running this
if(!require(integraFlora)) devtools::load_all()
library(plantR)

# Load user options
source("config.R")

# Prepare list of Locations
source("analyses/createUCsummary.R")

# Format data from each source
source("analyses/formatData/GBIF.R")
source("analyses/formatData/JABOT.R")
source("analyses/formatData/Reflora.R")
source("analyses/formatData/splink.R")
source("analyses/formatData/other.R")

# Join data and treat with plantR
source("analyses/joinData.R")
source("analyses/treatData.R")
# Remove duplicates
source("analyses/deduplicate.R")

# Clean old files
# source("analyses/cleanResults.R")
# Filter occs for each UC
source("analyses/getOccs.R")
# Generate checklists
source("analyses/treatOccs.R")
