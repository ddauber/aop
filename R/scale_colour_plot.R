#' Colour scale for Anatomy of Plots palettes
#'
#' @export
scale_colour_plot <- function(
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
      "colour",
      paste0("plot_", palette),
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_colour_plot
#' @export
scale_color_plot <- scale_colour_plot
