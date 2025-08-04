#' Base scale constructor for Anatomy of Plots palettes
#'
#' Internal utility for building `ggplot2` colour or fill scales using either
#' a palette name from the `aop_palettes` tibble or a user-supplied vector of
#' hex colours.
#'
#' @param type Either `"fill"` or `"colour"`, determining the aesthetic to scale.
#' @param palette Character string or vector. Name of a palette in `aop_palettes` (e.g. `"sunset"`) or a character vector of hex colour codes.
#' @param discrete Logical. Whether to use a discrete scale (`TRUE`, default) or continuous (`FALSE`).
#' @param reverse Logical. Whether to reverse the palette order.
#' @param select_distinct Logical. If `TRUE`, maximises colour contrast for discrete palettes.
#' @param ... Additional arguments passed to `ggplot2::discrete_scale()` or `ggplot2::scale_*_gradientn()`.
#'
#' @return A `ggplot2` scale object for use in plots.
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

  # Retrieve palette using aop_palette() only for recognised palette names
  if (
    is.character(palette) &&
      length(palette) == 1 &&
      palette %in% aop_palettes$name
  ) {
    pal_vector <- aop_palette(palette)
    is_named_palette <- TRUE
  } else if (is.character(palette)) {
    pal_vector <- palette
    is_named_palette <- FALSE
  } else {
    stop(
      "Invalid `palette`: must be a palette name or a character vector of hex colour codes."
    )
  }

  if (
    !is.character(pal_vector) ||
      anyNA(pal_vector) ||
      any(!grepl("^#[0-9A-Fa-f]{6}$", pal_vector))
  ) {
    stop(
      "Invalid palette: must be a palette name or a character vector of hex colour codes."
    )
  }

  if (reverse) {
    pal_vector <- rev(pal_vector)
  }

  crp <- grDevices::colorRampPalette(pal_vector)

  pal_fn <- function(n) {
    if (select_distinct && discrete) {
      return(select_distinct_colours(pal_vector, n))
    }
    if (n <= length(pal_vector)) {
      pal_vector[1:n]
    } else {
      crp(n)
    }
  }

  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = type,
      scale_name = paste0("plot_", if (is_named_palette) palette else "custom"),
      palette = pal_fn,
      ...
    )
  } else {
    if (type == "fill") {
      ggplot2::scale_fill_gradientn(colours = crp(256), ...)
    } else {
      ggplot2::scale_colour_gradientn(colours = crp(256), ...)
    }
  }
}

#' Colour scale for Anatomy of Plots palettes
#'
#' Applies a custom colour scale using a named palette from the Anatomy of Plots package
#' or a user-supplied vector of hex colours. Works with both discrete and continuous
#' scales and supports reversing the palette order. Optionally, selects the most
#' perceptually distinct colours when fewer are needed.
#'
#' @inheritParams scale_plot_base
#'
#' @return A `ggplot2` scale object for use in plots.
#' @export
scale_colour_plot <- function(
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  select_distinct = FALSE,
  ...
) {
  scale_plot_base(
    type = "colour",
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    select_distinct = select_distinct,
    ...
  )
}

#' Fill scale for Anatomy of Plots palettes
#'
#' Applies a custom fill scale using a named palette from the Anatomy of Plots package
#' or a user-supplied vector of hex colours. Works with both discrete and continuous
#' scales and supports reversing the palette order. Optionally, selects the most
#' perceptually distinct colours when fewer are needed.
#'
#' @inheritParams scale_plot_base
#' @export
scale_fill_plot <- function(
  palette = "sunset",
  discrete = TRUE,
  reverse = FALSE,
  select_distinct = FALSE,
  ...
) {
  scale_plot_base(
    type = "fill",
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    select_distinct = select_distinct,
    ...
  )
}
