#' Select most visually distinct colours from a palette
#'
#' Uses perceptual colour distance (CIE Lab) to choose the most distinct
#' subset of colours from a palette. Helps ensure clarity for low-k aesthetics.
#'
#' @param palette A character vector of hex colour codes.
#' @param k Number of colours to select. Must be a single positive integer.
#' @return A character vector of `k` colours from `palette`.
#'         If `k` is 1, the first palette colour is returned.
#' @export

select_distinct_colours <- function(palette, k) {
  # Input validation
  if (!is.character(palette) || any(is.na(palette))) {
    cli::cli_abort("Palette must be a character vector of valid hex colours.")
  }
  if (length(palette) < 1) {
    cli::cli_abort("Palette must contain at least one valid colour.")
  }
  if (length(k) != 1 || !is.numeric(k) || is.na(k) || k != as.integer(k)) {
    cli::cli_abort("k must be a single positive integer.")
  }
  k <- as.integer(k)
  if (k <= 0) {
    cli::cli_abort("k must be a single positive integer.")
  }
  if (k == 1) {
    return(palette[1])
  }
  if (length(palette) < 2) {
    cli::cli_abort("Palette must contain at least two valid colours for k > 1.")
  }

  # Shortcut: if palette is shorter than k, interpolate
  if (k > length(palette)) {
    return(grDevices::colorRampPalette(palette)(k))
  }

  # Step 1: Convert palette to Lab space
  rgb <- t(grDevices::col2rgb(palette)) # convert hex → RGB
  lab <- farver::convert_colour(rgb, from = "rgb", to = "lab")

  ## Ensure the Lab matrix is numeric and contains no missing values
  if (!is.matrix(lab) || !is.numeric(lab) || anyNA(lab)) {
    cli::cli_abort("Converted Lab matrix must be numeric and free of NA values.")
  }

  # Step 2: Distance matrix
  dist_matrix <- as.matrix(farver::compare_colour(lab, lab, from_space = "lab"))
  diag(dist_matrix) <- NA

  # Step 3: Start with most distant pair
  idx <- which(dist_matrix == max(dist_matrix, na.rm = TRUE), arr.ind = TRUE)[
    1,
  ]
  selected <- unique(idx)

  # Step 4: Greedily add most distant remaining colours
  while (length(selected) < k) {
    remaining <- setdiff(seq_along(palette), selected)
    if (length(remaining) == 0) {
      break
    } # Avoid infinite loop if all are used

    scores <- sapply(remaining, function(i) {
      min(dist_matrix[i, selected])
    })

    next_idx <- remaining[which.max(scores)]
    selected <- c(selected, next_idx)
  }

  # Step 5: Return final selection
  palette[selected]
}
