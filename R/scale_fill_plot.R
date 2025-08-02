#' Fill scale for Anatomy of Plots palettes
#'
#' Applies a custom fill scale using a named palette from the Anatomy of Plots package.
#' Works with both discrete and continuous scales and supports reversing the palette order.
#'
#' @param palette Character string. The name of the palette to use (e.g. `"sunset"`).
#' @param discrete Logical. Whether to use a discrete scale (`TRUE`, default) or continuous (`FALSE`).
#' @param reverse Logical. Whether to reverse the order of colours in the palette.
#' @param ... Additional arguments passed to `ggplot2::discrete_scale()` or `ggplot2::scale_fill_gradientn()`.
#'
#' @return A `ggplot2` scale object for use in `ggplot2` plots.
#' @export
scale_fill_plot <- function(
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  pal <- plot_palette(
    palette,
    type = if (discrete) "discrete" else "continuous",
    reverse = reverse
  )

  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      paste0("plot_", palette),
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
