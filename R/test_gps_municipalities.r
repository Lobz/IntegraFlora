#' Plot municipality-level validation summary
#'
#' Compares GPS-based municipality assignment to name-based assignment and produces
#' summary diagnostics for a municipality of interest.
#' @param name Municipality name to evaluate.
#' @param plot Logical flag to generate plots.
#' @param save Logical flag to save the output plot.
#' @param refdf Reference occurrence table used for name-based assignment.
#' @return A named vector with summary counts for GPS and name-based checks.
plotMun <- function(name, plot = TRUE, save = TRUE, refdf = dt) {
    gps_filter <- points_muns[[name]]
    filtered_gps <- valid_points[gps_filter,]
    # othermuns <- unique(filtered_gps$municipality.correct)
    # plot(filtered_gps, add=T, col = "blue")
    name_filter <- which(startsWith((refdf$loc.correct), munis_plantR[name, "loc.correct"]))
    name_filter_gps <- which(startsWith((valid_points$loc.correct), munis_plantR[name, "loc.correct"]))
    filtered_name <- refdf[name_filter,]
    if(nrow(filtered_name) == 0) {
        print(paste("Zero matches:", name))
        print(sort(table(filtered_gps$municipality.correct)) )
    }

    # Summary from GPS
    total_gps <- nrow(filtered_gps)
    correct <- length(intersect(name_filter_gps, gps_filter))
    na <- sum(is.na(filtered_gps$municipality.correct))
    wrong <- total_gps - correct - na
    summ_gps <- c(total_gps=total_gps,correct=correct,wrong_name=wrong,name_not_av=na)

    # Summary from Name
    total_name <- nrow(filtered_name)
    wrong <- length(name_filter_gps) - correct
    na <- total_name - correct - wrong
    summ_name <- c(total_name=total_name,correct=correct,wrong_gps=wrong,gps_not_av=na)

    total <- total_gps + total_name
    # Plots
    if(plot && total > 0) {
        tryCatch( {

        if(save) {
            png(paste0("plots/municipios/", tolower(plantR::rmLatin(name)), ".png"), height=480, width=640)
        }
        if(total_gps > 0 && total_name > 0){
            par(mfrow=c(2,2))
        } else {
            par(mfrow=c(2,1))
        }
        if(total_name > 0) {
            plot(sp$geom, main=name)
            plot(st_geometry(shapes[name,]), add=T)
            plot(valid_points$geometry[name_filter_gps], col=rgb(0,0,1,0.1), pch = 4, add=T)
        }
        if(summ_gps["wrong_name"] > 0){
            barplot(sort(table(filtered_gps$municipality.correct[filtered_gps$municipality.correct != name]), decreasing = TRUE)[1:3], main="Top three wrong municipalities", las=1, horiz=T)
        }
        if(total_name > 0) {
            barplot(summ_name[2:4], main = "Coords of points filtered by municipality")
        }
        if(total_gps > 0){
            barplot(summ_gps[2:4], main="Municipality of points filtered by GPS")
        }
        },
        finally={
            dev.off()
        })
    }

    c(summ_gps, summ_name[c(1,3:4)])
}
