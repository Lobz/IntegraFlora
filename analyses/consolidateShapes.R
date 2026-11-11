if(!require(integraFlora)) devtools::load_all()
require(sf)

shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$slug <- slug(standardize_uc_name(shapes$nome_uc))
shapes <- subset(shapes, slug %in% ucs$slug)
shapes <- shapes[order(shapes$slug), ]
