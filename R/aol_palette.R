#' Retrieve colours from an aop palette
#'
#' @param name Palette name (e.g., "sunset")
#' @return Character vector of hex colour codes
#' @export
aop_palette <- function(name) {
  pal <- aop_palettes[[name]]
  if (is.null(pal)) {
    stop(sprintf("Palette '%s' not found", name))
  }
  pal
}
