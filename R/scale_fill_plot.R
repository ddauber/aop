#' Fill scale for Anatomy of Plots palettes
#'
#' Applies a custom fill scale using a named palette from the Anatomy of Plots package
#' or a user-supplied vector of hex colours. Works with both discrete and continuous
#' scales and supports reversing the palette order. Optionally, selects the most
#' perceptually distinct colours when fewer are needed.
#'
#' @param palette Character string or vector. Name of a built-in palette (e.g. `"sunset"`)
#' or a character vector of hex colours.
#' @param discrete Logical. Whether to use a discrete scale (`TRUE`, default) or continuous (`FALSE`).
#' @param reverse Logical. Whether to reverse the palette order.
#' @param select_distinct Logical. If `TRUE`, maximises colour contrast for discrete palettes.
#' @param ... Additional arguments passed to `ggplot2::discrete_scale()` or `ggplot2::scale_fill_gradientn()`.
#'
#' @return A `ggplot2` scale object for use in `ggplot2` plots.
#' @export
scale_fill_plot <- function(
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  select_distinct = FALSE,
  ...
) {
  # Use user-supplied hex vector directly if not a named palette
  is_named_palette <- is.character(palette) &&
    length(palette) == 1 &&
    palette %in% names(aop_palettes)

  pal_vector <- if (is_named_palette) {
    aop_palettes[[palette]]
  } else {
    palette
  }

  if (!is.character(pal_vector) || anyNA(pal_vector)) {
    stop(
      "Invalid palette: must be a named palette or a character vector of hex codes."
    )
  }

  if (reverse) {
    pal_vector <- rev(pal_vector)
  }

  # Construct palette function
  pal_fn <- function(n) {
    if (select_distinct && discrete) {
      return(select_distinct_colours(pal_vector, n))
    }
    if (n <= length(pal_vector)) {
      pal_vector[1:n]
    } else {
      grDevices::colorRampPalette(pal_vector)(n)
    }
  }

  # Return appropriate scale
  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      paste0("plot_", if (is_named_palette) palette else "custom"),
      palette = pal_fn,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colours = grDevices::colorRampPalette(pal_vector)(256),
      ...
    )
  }
}
