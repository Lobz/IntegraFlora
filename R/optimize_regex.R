
#' Combine several alternative search strings into one regex
#'
#' This helper takes a vector of candidate strings and creates a single regular
#' expression that matches any of them. The result is suitable for use with
#' \code{grepl()} and related pattern-matching operations.
#'
#' @param x A character vector containing alternative strings to match.
#' @param optimize Logical indicating whether the regex should be optimized to
#'   reduce redundancy. Defaults to \code{FALSE}.
#' @return A single regex string that matches any of the strings in \code{x}.
#' @export
combineSearchStrings <- function(x, optimize = FALSE) {
    # Generate a regex string that matches all strings in x
    x <- tolower(unique(x))
    if (!optimize) {
        return(paste0("(", paste(generate_uc_string(x), collapse = "|"), ")"))
    }
    st <- generate_uc_string(x[1])
    while(TRUE) {
        # print(x)
        (m <- grepl(st, rmLatin(x)))
        # discard matched strings
        x <- x[!m]
        if (length(x) > 0) {
            # pick one string
            y <- generate_uc_string(x[1])
            # check if this string is better than the previous one
            # if(grepl(y, st)) {
                # st <- y
            # } else {
                st <- paste0(st, "|", y)
            # }
        } else {
            return(st)
        }
    }
}

combine_uc_regex <- function(x) {

  if (!is.character(x)) {
    stop("x must be a character vector.")
  }

  if (anyNA(x)) {
    stop("x must not contain NA values.")
  }

  if (any(!nzchar(x))) {
    x <- x[nzchar(x)]
  }

  # Remove duplicate names
  x <- unique(enc2utf8(x))

  # Return a regex that matches nothing
  if (length(x) == 0L) {
    return("(?!)")
  }

  # Split strings into characters
  chars <- strsplit(x, "", fixed = TRUE)

  # Escape regex metacharacters
  escape_regex <- function(x) {
    special <- c(
      "\\", ".", "^", "$", "|", "(", ")",
      "[", "]", "{", "}", "*", "+", "?"
    )

    paste0(
      ifelse(x %in% special, paste0("\\", x), x),
      collapse = ""
    )
  }

  # Find longest common prefix
  n <- min(lengths(chars))
  p <- 0L

  while (p < n) {
    next_chars <- vapply(
      chars, function(z) z[p + 1L], character(1)
    )

    if (!all(next_chars == next_chars[1L])) break

    p <- p + 1L
  }

  prefix <- if (p > 0L) {
    paste0(chars[[1]][seq_len(p)], collapse = "")
  } else {
    ""
  }

  # Remove common prefix
  remaining <- lapply(chars, function(z) {
    if (p > 0L) z[-seq_len(p)] else z
  })

  # Find longest common suffix
  n <- min(lengths(remaining))
  s <- 0L

  while (s < n) {

    next_chars <- vapply(
      seq_along(remaining),
      function(i) {
        z <- remaining[[i]]
        z[length(z) - s]
      },
      character(1)
    )

    if (!all(next_chars == next_chars[1L])) break

    s <- s + 1L
  }

  suffix <- if (s > 0L) {
    paste0(tail(remaining[[1]], s), collapse = "")
  } else {
    ""
  }

  # Remove common suffix
  middle <- lapply(remaining, function(z) {
    if (s > 0L) {
      z[seq_len(length(z) - s)]
    } else {
      z
    }
  })

  # Recursively construct a prefix trie
  build_trie <- function(parts) {

    lens <- lengths(parts)

    # A string ending at this node is a valid alternative
    terminal <- any(lens == 0L)

    parts <- parts[lens > 0L]

    if (length(parts) == 0L) {
      return("")
    }

    # Identify distinct next characters
    first <- vapply(
      parts, function(z) z[1L], character(1)
    )

    keys <- unique(first)

    # Construct each branch recursively
    branches <- vapply(keys, function(ch) {

      idx <- which(first == ch)

      rest <- lapply(parts[idx], function(z) z[-1L])

      paste0(
        escape_regex(ch),
        build_trie(rest)
      )

    }, character(1))

    # Include an empty alternative for terminal nodes
    if (terminal) {
      branches <- c("", branches)
    }

    # Group alternatives only when necessary
    if (length(branches) == 1L) {
      return(branches)
    }

    paste0("(", paste(branches, collapse = "|"), ")")
  }

  # Assemble final regex
  res <- paste0(
    escape_regex(prefix),
    build_trie(middle),
    escape_regex(suffix)
  )

  generate_uc_string(res)
}
