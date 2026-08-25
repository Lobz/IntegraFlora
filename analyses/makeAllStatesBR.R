devtools::load_all()
library(geobr)

results_folder <- "../ChecklistsBrazil/"

# Get relevant dataset info
datasets <- list_geobr()

# Read states data from geobr
st_info <- datasets[datasets[,1]=="read_state",]
st_latest_year <- sub(".* ","",st_info$year)
states <- geobr::read_state(year = st_latest_year)$name_state

head(states)
states <- sort(states)


# Create dirs
if(!dir.exists(results_folder)) {
    dir.create(results_folder)
}

for(x in states) {
    st_dir <- paste0(results_folder, slug(x))

    # make sure folder exists and is empty
    if(!dir.exists(st_dir)) {
        dir.create(st_dir)
    }

    # change conf
    system(paste("bash changeConf.sh", x))

    # copy tmp files to folder
    system(paste0("cp -n data-tmp/*.rda ", st_dir, "/"))

    # make
    system(paste0("make DATATMP=", st_dir, " RESULTS_DIR=", st_dir))
}
