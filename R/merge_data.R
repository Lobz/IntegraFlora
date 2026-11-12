#' Return a column from a data.frame or keep a literal value
#'
#' This helper is used to standardize field mapping when a metadata table lists
#' a column name but a given source may instead provide a fixed literal value.
#'
#' @param x A column name in the data.frame or a literal value to use when the
#'   column is not present.
#' @param dt A data.frame in which `x` may be a column name.
#' @return The requested column vector when present; otherwise the original
#'   literal `x`.
#' @export
column_or_value <- function(x, dt) {
    if (x %in% names(dt)) {
        return(dt[, x])
    }
    else {
        x
    }
}

#' Read and standardize one UC metadata file
#'
#' Reads a single source table using the file metadata stored in the parent
#' environment (`filenames` and `info_files`) and returns a harmonized UC-level
#' data.frame.
#'
#' @param i Integer index of the file to read in `filenames` and `info_files`.
#' @return A data.frame with standardized conservation-unit fields plus `type`
#'   and `slug` columns.
#' @export
formatInfo <- function(i) {
    tryCatch({
        print(paste("Reading ", filenames[i], "..."))
        dt_raw <- as.data.frame(data.table::fread(filenames[i], sep = info_files$sep[i], dec = info_files$dec[i], na.strings = c("", "NA")))
        dt <- data.frame(
            name = standardize_uc_name(column_or_value(info_files$nome_uc[i], dt_raw)),
            country = column_or_value(info_files$pais[i], dt_raw),
            stateProvince = column_or_value(info_files$estadoProvincia[i], dt_raw),
            municipality = column_or_value(info_files$municipio[i], dt_raw),
            protectionCategory = column_or_value(info_files$categoria[i], dt_raw),
            UC_ID = column_or_value(info_files$codigo[i], dt_raw),
            source = info_files$arquivo[i]
        )
        dt$type <- shorten_uc_name(stringr::str_extract(dt$name, paste(uc_abbrevs$long, collapse = "|")))
        dt$slug <- slug(dt$name)
        return(dt)
    },
    error = function(e) {
        print("Error reading file: ")
        warning(e)
        NULL
    })
}

#' Ensure the project UC helpers are loaded
#'
#' Internal helper to make the shape and UC standardization functions usable when
#' the file is sourced outside the package build context.
#'
#' @return Invisibly returns TRUE when the required helpers are available.
#' @export
.ensure_shape_helpers <- function() {
    required <- c("standardize_uc_name", "shorten_uc_name", "slug", "uc_abbrevs")
    missing <- required[!vapply(required, exists, logical(1), mode = "function")]
    if (length(missing) > 0L) {
        if (requireNamespace("devtools", quietly = TRUE)) {
            devtools::load_all(".", quiet = TRUE)
        } else {
            stop("Package helper functions are not available. Load the project with devtools::load_all() first.")
        }
    }
    invisible(TRUE)
}

#' Standardize an sf object using the shape metadata map
#'
#' Reads the metadata row in the shape descriptions CSV and standardizes the
#' column names and values of a spatial data.frame according to the project
#' naming conventions.
#'
#' @param x An sf object or data.frame to standardize.
#' @param i Integer row index or a file name/relative path in the metadata table.
#' @param info_files A data.frame read from `data-input/Locations/shapes/file_descriptions.csv`.
#' @return An sf object with standardized `name`, `country`, `stateProvince`,
#'   `municipality`, `protectionCategory`, `UC_ID`, `source`, `type`, and
#'   `slug` fields.
#' @export
formatShapeInfo <- function(x, i, info_files = NULL, remove_extra_cols = TRUE) {
    .ensure_shape_helpers()
    if (is.null(info_files)) {
        info_files <- read.csv("data-input/Locations/shapes/file_descriptions.csv",
            stringsAsFactors = FALSE, na.strings = c("", "NA"))
    }

    if (is.character(i) && length(i) == 1L) {
        info_row <- info_files[info_files$arquivo == i, , drop = FALSE]
    } else {
        info_row <- info_files[i, , drop = FALSE]
    }

    if (nrow(info_row) == 0L) {
        stop("No matching row found in file_descriptions.csv for the supplied shape reference.")
    }

    x_df <- as.data.frame(x)
    out <- x
    out$name <- standardize_uc_name(column_or_value(info_row$nome_uc[1], x_df))
    out$country <- column_or_value(info_row$pais[1], x_df)
    out$stateProvince <- column_or_value(info_row$estadoProvincia[1], x_df)
    out$municipality <- column_or_value(info_row$municipio[1], x_df)
    out$protectionCategory <- column_or_value(info_row$categoria[1], x_df)
    out$UC_ID <- column_or_value(info_row$codigo[1], x_df)
    out$source <- info_row$arquivo[1]
    out$type <- shorten_uc_name(stringr::str_extract(out$name, paste(uc_abbrevs$long, collapse = "|")))
    out$slug <- slug(out$name)

    if (inherits(x, "sf")) {
        out
    } else {
        as.data.frame(out)
    }

    if(remove_extra_cols) {
        keep_cols <- c("name", "country", "stateProvince", "municipality", "protectionCategory", "UC_ID", "source", "type", "slug")
        out <- out[, keep_cols, drop = FALSE]
    }

    # Return the standardized object
    sf::st_make_valid(out)
}


#' Read and standardize all shape files described in the metadata table
#'
#' Iterates over each row in the shapes metadata CSV, reads the corresponding .shp,
#' and standardizes the columns using column_or_value() and the project naming conventions.
#' @param info_files A data.frame read from data-input/Locations/shapes/file_descriptions.csv.
#' @param shapes_dir Directory containing the shape files.
#' @return A single sf data.frame with standardized metadata columns for all files.
#' @export
readShapeInfo <- function(info_files = NULL, shapes_dir = "data-input/Locations/shapes", remove_extra_cols = TRUE) {
    if (is.null(info_files)) {
        info_files <- read.csv(file.path(shapes_dir, "file_descriptions.csv"),
            stringsAsFactors = FALSE, na.strings = c("", "NA"))
    }

    col_names <- setdiff(names(info_files), "arquivo")

    files <- info_files$arquivo
    files <- files[!is.na(files) & nzchar(files)]

    shapes <- lapply(seq_along(files), function(i) {
        shp_path <- file.path(shapes_dir, files[i])
        if (!file.exists(shp_path)) {
            warning(paste("Shape file not found:", shp_path))
            return(NULL)
        }

        shape <- tryCatch(sf::st_read(shp_path, quiet = TRUE), error = function(e) NULL)
        if (is.null(shape)) {
            warning(paste("Could not read shape file:", shp_path))
            return(NULL)
        }

        formatShapeInfo(shape, i = i, info_files = info_files, remove_extra_cols = remove_extra_cols)
    })

    shapes
}

#' Merge a list of sf objects into a single sf object
#'
#' This function takes a list of sf objects and merges them into a single sf object, ensuring that the coordinate reference system (CRS) is consistent across all shapes. It also allows for specifying which columns to keep in the final merged object.
#'
#' @param shapes A list of sf objects to merge.
#' @param keep_cols A character vector of column names to retain in the merged object. If set to "all", all columns from the input shapes will be kept. Default is a predefined set of columns relevant to conservation units.
#' @param target_crs The target coordinate reference system (CRS) to which all shapes will be transformed. Default is "EPSG:4674".
#' @return A single sf object containing the merged shapes with the specified columns.
#' @export
mergeShapes <- function(shapes, keep_cols = c("name", "country", "stateProvince", "municipality", "protectionCategory", "UC_ID", "source", "type", "slug"), target_crs = "EPSG:4674") {
    # Filter out any NULL shapes from the list
    shapes <- Filter(Negate(is.null), shapes)
    if (length(shapes) == 0L) {
        return(NULL)
    }

    # Fix crs
    shapes <- lapply(shapes, function(s) {
        if (inherits(s, "sf") && !is.na(sf::st_crs(s))) {
            if (!identical(sf::st_crs(s), sf::st_crs(target_crs))) {
                s <- sf::st_transform(s, target_crs)
            }
        }
        s
    })

    all_cols <- unique(unlist(lapply(shapes, names), use.names = FALSE))
    if (length(keep_cols) == 1L && keep_cols == "all") {
        keep_cols <- all_cols
    } else {
        keep_cols <- intersect(keep_cols, all_cols)
    }
    shapes <- lapply(shapes, function(s) {
        missing <- setdiff(keep_cols, names(s))
        if (length(missing) > 0L) {
            for (nm in missing) {
                s[[nm]] <- NA
            }
        }
        s[, keep_cols, drop = FALSE]
    })

    do.call(rbind, shapes)
}

#' Summarize and clean a UC table
#'
#' Standardizes locality fields using the project naming conventions, filters the
#' records to the target state/province, and removes duplicated conservation-unit
#' names.
#'
#' @param dt A data.frame containing conservation-unit metadata, including at
#'   least `name`, `country`, `stateProvince`, and `municipality` columns.
#' @return A cleaned data.frame with corrected locality fields and duplicate
#'   entries removed.
#' @export
cleanLocFields <- function(dt, subsetTo = NULL, removeDuplicates = FALSE) {
    dtl <- dt
    dtl$locality <- dt$name
    dtl <- addAdmin(formatLoc(dtl))
    dt$country <- ifelse(is.na(dtl$country.correct), dt$country,
        dtl$country.correct)
    dt$stateProvince <- ifelse(is.na(dtl$stateProvince.correct),
        dt$stateProvince, dtl$stateProvince.correct)
    dt$municipality <- ifelse(is.na(dtl$municipality.correct),
        dt$municipality, dtl$municipality.correct)

    if(!is.null(subsetTo)) {
        dt <- dt[grep(subsetTo, dt$stateProvince), ]
    }
    if (anyDuplicated(dt$name)) {
        warning(paste(c("Found UCs with duplicated names:", shorten_uc_name(dt$name[duplicated(dt$name)])),
            collapse = "\n"))
        if(removeDuplicates) {
            warning("Removing duplicated names, keeping the first occurrence.")
            dt <- dt[!duplicated(dt$name) & !is.na(dt$name), ]
        }
    }
    return(dt)
}

#' Merge a list of UC tables
#'
#' Keeps records with matching slugs or highly similar names while removing
#' redundant entries from the pairwise comparison.
#'
#' @param data_list A list of UC data.frames.
#' @param filenames Optional vector of source names used in logs.
#' @param merge.function Merge function to apply between tables.
#' @return A single merged UC data.frame.
#' @export
mergeInfo <- function(data_list, filenames = names(data_list)) {
    if (length(data_list) == 0) {
        stop("No data frames provided for merging.")
    }
    if (length(data_list) == 1) {
        return(data_list[[1]])
    }

    all_ucs <- do.call(rbind, data_list)
    if (anyDuplicated(all_ucs$slug)) {
        print("Found UCs with duplicated slugs across sources. Merging duplicates...")
        all_ucs <- all_ucs[!duplicated(all_ucs$slug), ]
    }

    all_ucs$locality <- all_ucs$name
    all_ucs <- fixLocation(all_ucs)

    # Prepare for searching alternative names in the occurrence data
    checkedLocations <- read.csv("data-input/Locations/extraTables/checkedLocations.csv")
    checkedLocations$slug <- toupper(slug(standardize_uc_name(checkedLocations$Nome_UC)))
    altNames <- consolidateNamesTable(checkedLocations = checkedLocations, extraNames = unique((all_ucs$name)))

    duplicates <- nameSearch(ucs = all_ucs$names, corpus = all_ucs, LT = altNames, fieldA = "name", fieldB = "")

    skip <- rep(FALSE, nrow(all_ucs))
    for (i in 1:nrow(all_ucs)) {
        id <- all_ucs$slug[i]
        # Don't skip ucs in checkedLocations.csv
        if (id %in% checkedLocations$slug) skip[i] <- FALSE
        if (skip[i]) next

        # Skip duplicates for the current UC
        dups <- duplicates[[id]]
        dups[i] <- FALSE  # Exclude self
        # Exclude UCs with different types
        type0 <- all_ucs$type[i]
        dups[all_ucs$type[dups] != type0] <- FALSE
        if(any(dups)) {
            print(paste("Merging duplicates for UC", i, all_ucs$slug[i]))
            cat("Duplicates found: ")
            cat(paste(all_ucs$slug[dups], collapse = ", "), "\n")
            skip[dups] <- TRUE
        }
    }
    dont_skip <- all_ucs$slug %in% checkedLocations$slug
    if(any(dont_skip & skip)) {
        print(paste("Ensuring checked locations are not skipped:", paste(all_ucs$slug[dont_skip & skip], collapse = ", ")))
    }
    skip[dont_skip] <- FALSE  # Ensure checked locations are not skipped
    all_ucs <- all_ucs[!skip, ]
    return(all_ucs)
}

#' Subset spatial records by state
#'
#' Intersects the supplied shape object with the geobr state polygons and returns
#' only the portions falling inside the requested state or all states when no
#' filter is supplied.
#'
#' @param shape An sf object of conservation-unit polygons.
#' @param subsetToState Optional state name to filter the result to.
#' @return An sf object with state membership and area proportions added.
#' @export
splitIntoStates <- function(shape, subsetToState = "") {

    # We will NOT be trusting the cnuc text data for what municipalities are included in each UC
    # Instead, we will use the shape data and compare it with geobr data

    # Get relevant dataset info
    datasets <- geobr::list_geobr()

    # Read state data from geobr
    state_years <- datasets[datasets[,1]=="read_state", "year"]
    state_latest_year <- sub(".*, ", "",state_years)
    state <- geobr::read_state(year = state_latest_year)

    if(subsetToState != "") {
        state <- state[state$name_state == subsetToState, ]
    }

    # Add area column to the shape data
    shape$area_calc <- sf::st_area(shape)

    # Intersect state shapes with UC shapes
    inter_state <- sf::st_intersection(state, shape)

    # Filter areas that are too small
    inter_state$area_calc2 <- sf::st_area(inter_state)
    inter_state$area_prop <- as.numeric(inter_state$area_calc2/inter_state$area_calc)
    inter_state <- subset(inter_state, area_prop > 0.05)

    # Add stateProvince column to the intersected data
    if("stateProvince" %in% names(inter_state)) {
        inter_state$verbatimStateProvince <- inter_state$stateProvince
    }
    inter_state[["stateProvince"]] <- inter_state$name_state

    # return
    return(inter_state)
}
