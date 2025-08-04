#' Generate a palette function
#'
#' @param name Name of the palette
#' @param type "discrete" or "continuous"
#' @param reverse Reverse palette order?
#' @return A function(n) returning n colours
#' @keywords internal

plot_palette <- function(
  name,
  type = c("discrete", "continuous"),
  reverse = FALSE,
  ensure_contrast = FALSE
) {
  type <- match.arg(type)
  pal <- aop_palettes[[name]]
  if (is.null(pal)) {
    stop("Palette not found.")
  }
  if (reverse) {
    pal <- rev(pal)
  }

  function(n) {
    if (ensure_contrast && type == "discrete") {
      return(select_distinct_colours(pal, n))
    }

    if (type == "continuous") {
      grDevices::colorRampPalette(pal)(n)
    } else {
      if (n <= length(pal)) {
        pal[1:n]
      } else {
        grDevices::colorRampPalette(pal)(n)
      }
    }
  }
}
