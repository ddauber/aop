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
#' @keywords internal
scale_plot_base <- function(
  type = c("fill", "colour"),
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  select_distinct = FALSE,
  ...
) {
  type <- match.arg(type)

  # Retrieve palette using aop_palette(); fall back to supplied vector if not found
  pal_info <- tryCatch(
    list(palette = aop_palette(palette), is_named = TRUE),
    error = function(e) list(palette = palette, is_named = FALSE)
  )
  pal_vector <- pal_info$palette
  is_named_palette <- pal_info$is_named

  if (
    !is.character(pal_vector) ||
      anyNA(pal_vector) ||
      any(!grepl("^#[0-9A-Fa-f]{6}$", pal_vector))
  ) {
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

  # Return appropriate ggplot2 scale
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = type,
      scale_name = paste0("plot_", if (is_named_palette) palette else "custom"),
      palette = pal_fn,
      ...
    )
  } else {
    if (type == "fill") {
      ggplot2::scale_fill_gradientn(
        colours = grDevices::colorRampPalette(pal_vector)(256),
        ...
      )
    } else {
      ggplot2::scale_colour_gradientn(
        colours = grDevices::colorRampPalette(pal_vector)(256),
        ...
      )
    }
  }
}

#' Colour scale for Anatomy of Plots palettes
#'
#' @inheritParams scale_plot_base
#' @export
scale_colour_plot <- function(...) {
  scale_plot_base(type = "colour", ...)
}

#' Fill scale for Anatomy of Plots palettes
#'
#' @inheritParams scale_plot_base
#' @export
scale_fill_plot <- function(...) {
  scale_plot_base(type = "fill", ...)
}
