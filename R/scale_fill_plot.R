#' Colour scale for Anatomy of Plots palettes
#'
#' Applies a custom fill scale using a named palette from the Anatomy of Plots package
#' or a user-supplied palette vector. Supports both discrete and continuous scales.
#'
#' @param palette Character string (name of palette) or vector of hex codes.
#' @param discrete Logical. Use a discrete scale (TRUE, default) or continuous (FALSE).
#' @param reverse Logical. Whether to reverse the palette.
#' @param select_distinct Logical. If TRUE, select most distinct fills (for discrete only).
#' @param ... Additional arguments passed to ggplot2 scale functions.
#'
#' @return A ggplot2 scale object
#' @export
scale_fill_plot <- function(
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  select_distinct = FALSE,
  ...
) {
  raw_pal <- if (
    is.character(palette) &&
      length(palette) == 1 &&
      palette %in% names(aop_palettes)
  ) {
    plot_palette(
      palette,
      type = if (discrete) "discrete" else "continuous",
      reverse = reverse
    )
  } else if (is.character(palette) && all(grepl("^#", palette))) {
    pal <- palette
    if (reverse) rev(pal) else pal
  } else {
    stop(
      "Invalid `palette` input. Must be a named palette or a vector of fill hex codes."
    )
  }

  if (discrete) {
    pal <- function(n) {
      if (select_distinct && n < length(raw_pal)) {
        return(select_distinct_colours(raw_pal, k = n))
      }
      if (n <= length(raw_pal)) {
        return(raw_pal[1:n])
      }
      grDevices::colorRampPalette(raw_pal)(n)
    }

    ggplot2::discrete_scale(
      "fill",
      paste0("plot_", as.character(palette)[1]),
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      fills = grDevices::colorRampPalette(raw_pal)(256),
      ...
    )
  }
}

#' @rdname scale_fill_plot
#' @export
scale_fill_plot <- scale_fill_plot
