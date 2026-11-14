#' Corrections
#'
#' Apply correction tables described in a metadata CSV under
#' data-input/Occurrences/Corrections.
#'
#' The metadata file must contain three columns:
#' \code{arquivo}, \code{identificadores}, and \code{dado_corrigido}.
#' The \code{identificadores} and \code{dado_corrigido} values may each contain
#' a comma-separated list of column names.
#'
#' @param x A data.frame containing the records to update.
#' @param corrections_dir Directory with the metadata file and the correction CSVs.
#' @param descriptions_file Metadata CSV containing the file-to-correction map.
#' @param ... Extra arguments passed to \code{read.csv}.
#' @return A data.frame with corrected values applied.
#' @export
apply_corrections <- function(x, corrections_dir = "data-input/Occurrences/Corrections",
                             descriptions_file = "file_descriptions.csv", ...) {
    if (!is.data.frame(x)) {
        stop("'x' must be a data.frame.")
    }

    file_path <- file.path(corrections_dir, descriptions_file)
    if (!file.exists(file_path)) {
        stop("Correction metadata file not found: ", file_path)
    }

    # Read the metadata table that maps each correction CSV to its matching keys
    # and the fields to replace.
    metadata <- utils::read.csv(file_path, stringsAsFactors = FALSE,
        na.strings = c("", "NA"), ...)

    # Every row must include the file name, identifier columns, and target field(s).
    required_cols <- c("arquivo", "identificadores", "dado_corrigido")
    if (!all(required_cols %in% names(metadata))) {
        stop("Correction metadata file must contain the columns: ",
             paste(required_cols, collapse = ", "))
    }

    # Apply each correction file in turn to the same working data.frame.
    for (i in seq_len(nrow(metadata))) {
        row <- metadata[i, , drop = FALSE]
        file_name <- trimws(as.character(row$arquivo[1]))
        if (is.na(file_name) || !nzchar(file_name)) {
            next
        }

        # Locate and validate the correction CSV before use.
        correction_path <- file.path(corrections_dir, file_name)
        if (!file.exists(correction_path)) {
            warning("Correction file not found: ", correction_path)
            next
        }

        # Allow the metadata to store one or many identifier columns and target
        # columns as a comma-separated string.
        id_cols <- .correction_field_list(row$identificadores[1])
        target_cols <- .correction_field_list(row$dado_corrigido[1])

        if (length(id_cols) == 0L || length(target_cols) == 0L) {
            warning("Skipping malformed correction metadata row: ", file_name)
            next
        }

        # Read the actual correction values and apply them to the main dataset.
        correction_dt <- utils::read.csv(correction_path, stringsAsFactors = FALSE,
            na.strings = c("", "NA"), ...)
        x <- apply_single_correction(x, correction_dt = correction_dt,
                                    id_cols = id_cols, target_cols = target_cols)
    }

    x
}

#' Apply a single correction table to a data.frame.
#'
#' @param x A data.frame containing the records to update.
#' @param correction_dt A data.frame containing the rows to use as corrections.
#' @param id_cols Character vector of identifier columns used to match records.
#' @param target_cols Character vector of columns to overwrite with corrected values.
#' @return A data.frame with corrected values applied.
#' @export
apply_single_correction <- function(x, correction_dt, id_cols, target_cols) {
    # Validate the main object and the correction table before matching.
    if (!is.data.frame(x)) {
        stop("'x' must be a data.frame.")
    }
    if (!is.data.frame(correction_dt)) {
        stop("'correction_dt' must be a data.frame.")
    }
    if (length(id_cols) == 0L || length(target_cols) == 0L) {
        warning("Skipping malformed correction specification.")
        return(x)
    }
    if (!all(id_cols %in% names(x))) {
        stop("The data frame is missing identifier columns: ",
             paste(setdiff(id_cols, names(x)), collapse = ", "))
    }
    if (!all(id_cols %in% names(correction_dt))) {
        stop("Correction data.frame is missing identifier columns: ",
             paste(setdiff(id_cols, names(correction_dt)), collapse = ", "))
    }
    missing_targets <- setdiff(target_cols, names(correction_dt))
    if (length(missing_targets) > 0L) {
        stop("Correction data.frame does not contain the target fields: ",
             paste(missing_targets, collapse = ", "))
    }

    # Ensure all target columns exist in the working data.frame before writing.
    missing_targets <- setdiff(target_cols, names(x))
    if(length(missing_targets)>0) {
        warning("Data is missing target fields: ",
            paste(missing_targets, collapse = ", "))
        x[]
    }
    for (target_col in missing_targets) {
        x[[target_col]] <- NA
    }

    # Build a composite key so multi-column identifiers can be matched reliably.
    ref_key <- .correction_key(correction_dt[, id_cols, drop = FALSE])
    x_key <- .correction_key(x[, id_cols, drop = FALSE])
    matches <- match(x_key, ref_key)

    # Replace only matching rows and only the requested target columns.
    to_update <- !is.na(matches)
    if (any(to_update)) {
        for (target_col in target_cols) {
            x[to_update, target_col] <- correction_dt[[target_col]][matches[to_update]]
        }
    }

    x
}

#' Parse a CSV cell containing a comma-separated list of column names.
#'
#' The metadata files store lists like "barcode" or "collectionCode,catalogNumber"
#' as a single cell. This helper splits and normalizes those values.
#'
#' @param x A character vector or scalar describing one or more column names.
#' @return A character vector with whitespace trimmed and empty entries removed.
.correction_field_list <- function(x) {
    # An empty or missing cell means there are no identifiers/targets to parse.
    if (length(x) == 0L || all(is.na(x))) {
        return(character(0))
    }

    # Split comma-separated column names and trim whitespace around each entry.
    raw <- unlist(strsplit(trimws(as.character(x)), ",", fixed = TRUE))
    raw <- trimws(raw)
    raw[nzchar(raw)]
}

#' Construct a stable matching key from one or more identifier columns.
#'
#' We concatenate the values from each identifier column using a separator so
#' composite keys remain unique and comparable even when the keys include
#' characters such as commas or spaces.
#'
#' @param dt A data.frame containing identifier columns.
#' @return A character vector of composite keys.
.correction_key <- function(dt) {
    if (nrow(dt) == 0L) {
        return(character(0))
    }

    # Fold each identifier column into a single string key, preserving missing
    # values as NA and joining columns with a separator unlikely to appear.
    keys <- lapply(seq_len(ncol(dt)), function(j) {
        x <- dt[[j]]
        if (is.factor(x)) {
            x <- as.character(x)
        }
        ifelse(is.na(x), NA_character_, as.character(x))
    })

    do.call(paste, c(keys, sep = "\r"))
}