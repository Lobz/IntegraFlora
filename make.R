# Please read the README before running this
devtools::load_all()

# Prepare list of Locations
source("analyses/createUCsummary.R")
# Make gazetteer
source("analyses/createUCgazetteer.R")

# Format data from each source
source("analyses/formatData/GBIF.R")
source("analyses/formatData/JABOT.R")
source("analyses/formatData/Reflora.R")
source("analyses/formatData/splink.R")
source("analyses/formatData/other.R")

# Join data and treat with plantR
source("analyses/joinData.R")
# Remnove duplicates
source("analyses/deduplicate.R")

# Filter occs for each UC
source("analyses/getOccs.R")
# Generate checklists
source("analyses/treatOccs.R")
