#' Custom fill scale from user-supplied palette
#'
#' Applies a user-defined palette to the fill aesthetic in ggplot2.
#'
#' @param palette A character vector of colours (hex codes or names)
#' @param discrete Whether to treat the scale as discrete (default TRUE)
#' @param reverse Whether to reverse the palette order (default FALSE)
#' @param ... Additional arguments passed to the ggplot2 scale function
#'
#' @return A ggplot2 fill scale
#' @export
#' @rdname scale_fill_custom
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

#' Custom colour scale from user-supplied palette
#'
#' Applies a user-defined palette to the colour aesthetic in ggplot2.
#'
#' @inheritParams scale_fill_custom
#'
#' @return A ggplot2 colour scale
#' @export
#' @rdname scale_colour_custom
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

#' @rdname scale_colour_custom
#' @export
scale_color_custom <- scale_colour_custom
