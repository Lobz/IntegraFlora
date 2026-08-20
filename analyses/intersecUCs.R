if(!require(integraFlora)) devtools::load_all()
library(stringr)
library(parallel)
library(sf)

# Shape data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)
shapes <- shapes[order(shapes$nome_uc), ]
shapes_buff <- st_buffer(shapes, 100)

# Intersect shapes with shapes
shapes_intersect <- st_intersects(shapes)
shapes_covers <- st_covers(shapes)
shapes_covers_buffer <- st_covers(shapes_buff, shapes)

# Remove from broader list what is already on stricter list
shapes_intersect <- pairwiseMap(shapes_intersect, shapes_covers, setdiff)
shapes_covers_buffer <- pairwiseMap(shapes_covers_buffer, shapes_covers, setdiff)

# Remove self
shapes_covers <- lapply(shapes_covers, function(x) x[-1])

# Correct type
shapes_covers_buffer <- lapply(shapes_covers_buffer, unlist)
shapes_intersect <- lapply(shapes_intersect, unlist)

# test
plot(st_geometry(shapes[i,]))
plot(st_geometry(shapes[shapes_intersect[[i]],]), add=T, col=shapes_covers_buffer[[i]])
plot(st_geometry(shapes[i,]), col =2)
plot(st_geometry(shapes_buff[i,]), add=T)

plot(st_geometry(shapes[c(113,16),]))

# Get areas
shapes$area <- units::set_units(st_area(shapes),ha)

# Create report
coverage <- lapply(1:nrow(shapes), function(i) {
# for(i in 1:nrow(shapes)) {
    name <- shapes$nome_uc[i]
    covered <- shapes$nome_uc[shapes_covers[[i]]]
    covered_buff <- shapes$nome_uc[shapes_covers_buffer[[i]]]
    my_intersections <- shapes_intersect[[i]]

    df <- data.frame(nome_uc = rep(name, length(covered)+length(covered_buff)),
                     outra_uc = c(covered, covered_buff),
                     status = c(rep("covered", length(covered)), rep("covered_buffer", length(covered_buff))))


    inters <- st_intersection(
                shapes$geometry[i],
                shapes$geometry[my_intersections],
                model = "closed" # allow for sharing of boundaries
            )
    area <- units::set_units(st_area(inters),ha)
    prop <- area/(shapes$area[my_intersections])

    df_i <- data.frame(nome_uc = rep(name, length(my_intersections)),
        outra_uc = shapes$nome_uc[my_intersections],
        prop = round(100*prop))
    df <- merge(df, df_i, all=T)
    df$status[is.na(df$status)] = "intersect"

    df
}
)

which(shapes$nome_uc=="ÁREA DE PROTEÇÃO AMBIENTAL MARINHA DO LITORAL NORTE")

coverage <- do.call(rbind, coverage)
coverage$status <- as.factor(coverage$status)
coverage$prop[coverage$status=="covered"] <- 100
coverage$prop[is.na(coverage$prop)] <- 0
summary(coverage)

coverage[coverage$status=="covered_buffer",]
summary(coverage[coverage$status=="covered_buffer",])
summary(coverage[coverage$status=="intersect",])

write.csv(coverage, "data-input/Locations/extraTables/intersecUCs.csv")
