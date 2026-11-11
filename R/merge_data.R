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
#' @keywords internal
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
merge_shapes <- function(shapes, keep_cols = c("name", "country", "stateProvince", "municipality", "protectionCategory", "UC_ID", "source", "type", "slug"), target_crs = "EPSG:4674") {
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

#' Merge duplicate UC entries from two sources
#'
#' Keeps records with matching slugs or highly similar names while removing
#' redundant entries from the pairwise comparison. This is the core duplicate-
#' resolution step used when combining UC tables from different sources.
#'
#' @param A A data.frame of UC entries.
#' @param B A second data.frame of UC entries.
#' @return A merged data.frame without redundant duplicates.
#' @export
merge_info <- function(A, B, searchWith = NULL) {

    if(any(is.na(A$slug)) || any(is.na(B$slug))) {
        warning("Some entries have NA slugs. These will be removed.")
        A <- A[!is.na(A$slug), ]
        B <- B[!is.na(B$slug), ]
    }

    repA <- A$slug %in% B$slug
    repB <- B$slug %in% A$slug
    r <- A[repA, ]
    A <- A[!repA, ]
    B <- B[!repB, ]

    if(is.null(searchWith)) {
        strA <- paste0("^", generate_uc_string(A$name))
        strB <- paste0("^", generate_uc_string(B$name), "$")
    } else {
        strA <- searchWith$x[match(A$slug, searchWith$slug)]
        strB <- searchWith$x[match(B$slug, searchWith$slug)]
    }

    matchA <- sapply(strA, function(s) {
        any(grepl(s, x = rmLatin(B$name), ignore.case = T))
    })
    if (any(matchA)) {
        r <- rbind(r, A[matchA, ])
        A <- A[!matchA, ]
        whichA <- unlist(sapply(strA[matchA], grep, x = rmLatin(B$name),
            ignore.case = T))
        B <- B[-whichA, ]
    }
    matchB <- sapply(strB, function(s) {
        any(grepl(s, x = rmLatin(A$name), ignore.case = T))
    })
    if (any(matchB)) {
        r <- rbind(r, B[matchB, ])
        B <- B[!matchB, ]
        whichB <- unlist(sapply(strB[matchB], grep, x = rmLatin(A$name),
            ignore.case = T))
        A <- A[-whichB, ]
    }
    r <- rbind(r, A, B)
    return(r)
}

#' Merge a list of UC tables
#'
#' Sequentially merges a collection of UC summary tables using the custom
#' similarity-based merge function.
#' @param data_list A list of UC data.frames.
#' @param filenames Optional vector of source names used in logs.
#' @param merge.function Merge function to apply between tables.
#' @return A single merged UC data.frame.
#' @export
merge_many <- function(data_list, filenames = names(data_list),
    merge.function = merge_info, ...) {
    dt <- data_list[[1]]
    for (i in 2:length(data_list)) {
        print(paste("Merging", filenames[i], "..."))
        dt <- merge_info(dt, data_list[[i]], ...)
    }
    dt
}

splitIntoStates <- function(shape, state_col = "stateProvince") {

    # We will NOT be trusting the cnuc text data for what municipalities are included in each UC
    # Instead, we will use the shape data and compare it with geobr data

    # Get relevant dataset info
    datasets <- geobr::list_geobr()

    # Read state data from geobr
    state_years <- datasets[datasets[,1]=="read_state", "year"]
    state_latest_year <- sub(".*, ", "",state_years)
    state <- geobr::read_state(year = state_latest_year)
    head(state)


    shape$area_calc <- sf::st_area(shape)

    # Intersect state shapes with UC shapes
    inter_state <- sf::st_intersection(state, shape)
    head(inter_state)
    table(inter_state$name_state)

    # Filter areas that are too small
    inter_state$area_calc2 <- st_area(inter_state)
    inter_state$area_prop <- as.numeric(inter_state$area_calc2/inter_state$area_calc)
    boxplot(inter_state$area_prop~ inter_state$name_state)

    inter_state <- subset(inter_state, area_prop > 0.05)
    table(inter_state$name_state, useNA="always")

    table(inter_state$name_state)

}
