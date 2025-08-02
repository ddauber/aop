#' Fill scale for Anatomy of Plots palettes
#'
#' @export
scale_fill_plot <- function(palette = "sunset", discrete = TRUE, reverse = FALSE, ...) {
  pal <- plot_palette(palette, type = if (discrete) "discrete" else "continuous", reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("plot_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}