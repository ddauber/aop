#' Custom fill scale from user-supplied palette
#'
#' @param palette A vector of colours (hex codes or names)
#' @param discrete Whether to treat the scale as discrete or continuous
#' @param reverse Whether to reverse the palette order
#' @param ... Additional arguments passed to the scale
#'
#' @return A ggplot2 fill scale
#' @export
scale_fill_custom <- function(palette, discrete = TRUE, reverse = FALSE, ...) {
  if (!is.character(palette) || length(palette) < 2) {
    stop("Please provide a character vector of at least two colours.")
  }
  if (reverse) {
    palette <- rev(palette)
  }

  pal <- function(n) {
    if (discrete) {
      if (n <= length(palette)) {
        palette[1:n]
      } else {
        grDevices::colorRampPalette(palette)(n)
      }
    } else {
      grDevices::colorRampPalette(palette)(n)
    }
  }

  if (discrete) {
    ggplot2::discrete_scale("fill", "custom", palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @inheritParams scale_fill_custom
#' @export
scale_colour_custom <- function(
  palette,
  discrete = TRUE,
  reverse = FALSE,
  ...
) {
  if (!is.character(palette) || length(palette) < 2) {
    stop("Please provide a character vector of at least two colours.")
  }
  if (reverse) {
    palette <- rev(palette)
  }

  pal <- function(n) {
    if (discrete) {
      if (n <= length(palette)) {
        palette[1:n]
      } else {
        grDevices::colorRampPalette(palette)(n)
      }
    } else {
      grDevices::colorRampPalette(palette)(n)
    }
  }

  if (discrete) {
    ggplot2::discrete_scale("colour", "custom", palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' @export
scale_color_custom <- scale_colour_custom
