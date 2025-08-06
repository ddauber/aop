#' Retrieve colours from an Anatomy of Plots palette
#'
#' Looks up `name` in the `aop_palettes` tibble and returns the
#' corresponding vector of hex colour codes.
#'
#' @param name Palette name (e.g., "sunset")
#' @return Character vector of hex colour codes
#' @export
aop_palette <- function(name) {
  pal <- aop_palettes |> dplyr::filter(name == {{ name }})
  if (nrow(pal) == 0) {
    cli::cli_abort("Palette '{name}' not found")
  }
  if (nrow(pal) > 1) {
    cli::cli_abort("Multiple palettes named '{name}' found")
  }
  pal <- pal |> dplyr::pull(hex) |> purrr::pluck(1)
  pal
}

utils::globalVariables("hex")
