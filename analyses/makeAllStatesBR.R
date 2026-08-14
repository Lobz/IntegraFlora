devtools::load_all()
library(geobr)

results_folder <- "../ChecklistsBrazil/"

# Get relevant dataset info
datasets <- list_geobr()

# Read states data from geobr
st_info <- datasets[datasets[,1]=="read_state",]
st_latest_year <- sub(".* ","",st_info$year)
states <- geobr::read_state(year = st_latest_year)$name_state

write.csv(states, "states.txt",row.names=F, col.names=F)

head(states)



# Create dirs
if(!dir.exists(results_folder)) {
    dir.create(results_folder)
}

sapply(states, function(x) {
    st_dir <- paste0(results_folder, slug(x))

    # make sure folder exists and is empty
    if(!dir.exists(st_dir)) {
        dir.create(st_dir)
    } else {
        system(paste0("rm -rf ", st_dir, "/*"))
    }

    system(paste("bash makeAndMove.sh", x))

})
