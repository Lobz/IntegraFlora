if(!require(integraFlora)) devtools::load_all()
require(sf)

print("Loading multipolygons...")
folder <- "data-input/Locations/shapes"
shape_files <- list.files(folder, pattern = "*.shp", full.names = TRUE, recursive = TRUE)
shapes <- lapply(shape_files, readShape)

lapply(shapes, names)

shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$slug <- slug(standardize_uc_name(shapes$nome_uc))
shapes <- subset(shapes, slug %in% ucs$slug)
shapes <- shapes[order(shapes$slug), ]
