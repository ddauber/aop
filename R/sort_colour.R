#' Sort hex colours by hue and lightness
#'
#' This function takes a character vector of hexadecimal colour codes and sorts
#' them for aesthetic presentation based on either hue (default) or lightness.
#' It uses the HCL (hue-chroma-lightness) colour space for perceptual consistency.
#'
#' @param hex A character vector of hex colour codes (e.g., `#FF0000`).
#' @param priority A string indicating the sorting priority. Must be either
#'   "hue" (default) or "lightness".
#'
#' @return A character vector of hex colour codes sorted for aesthetic coherence.
#'
#' @examples
#' sort_colours(c("#FF0000", "#00FF00", "#0000FF"))
#' sort_colours(c("#FF0000", "#00FF00", "#0000FF"), priority = "lightness")
#'
#' @export
sort_colours <- function(hex, priority = c("hue", "lightness")) {
  priority <- match.arg(priority)

  if (!is.character(hex) || anyNA(hex)) {
    cli::cli_abort("Input must be a character vector of hex colours.")
  }

  rgb <- t(grDevices::col2rgb(hex))
  hcl <- farver::convert_colour(rgb, from = "rgb", to = "hcl")

  order_index <- switch(
    priority,
    hue = order(hcl[, 1], hcl[, 3]), # hue, then lightness
    lightness = order(hcl[, 3], hcl[, 1]) # lightness, then hue
  )

  hex[order_index]
}

#' @rdname sort_colours
#' @export
sort_colors <- sort_colours
