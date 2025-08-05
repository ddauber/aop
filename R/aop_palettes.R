#' Colour palettes for Anatomy of Plots
#'
#' This tibble contains palette name, mode, mood, tags, and hex codes.
#' Each palette is a named character vector of hex codes.
#'
#' @format A tibble with one row per palette and the following columns:
#' \describe{
#'   \item{id}{Unique numeric ID for the palette}
#'   \item{name}{Name of the palette (e.g. "sunset")}
#'   \item{length}{Number of colours in the palette}
#'   \item{mode}{Best suited background: "dark", "light", or "both"}
#'   \item{mood}{Subjective aesthetic theme, e.g. "warm", "cool", "vibrant"}
#'   \item{tags}{Character vector of keywords for filtering (e.g. "gradient", "space")}
#'   \item{hex}{Character vector of hex colour codes}
#' }
#' @keywords internal
aop_palettes <- tibble::tibble(
  id = c(1, 2, 3, 4, 5, 6),
  name = c(
    "sunset_crimson_warm",
    "ocean_muted_cool",
    "forest_muted_earthy",
    "violetstorm_muted_moody",
    "sunrise_teal_gentle",
    "sage_teal_earthy"
  ),
  length = c(5, 4, 4, 5, 4, 4),
  mode = c("both", "dark", "light", "dark", "both", "both"),
  mood = c("warm", "cool", "earthy", "moody", "gentle", "earthy"),
  tags = list(
    c("citrus", "bright"),
    c("marine", "deep"),
    c("nature", "calm"),
    c("purple", "storm"),
    c("dawn", "soft"),
    c("nature", "soft", "bright")
  ),
  hex = list(
    c("#FF5733", "#FF8D1A", "#FFC300", "#DAF7A6", "#C70039"),
    c("#001F3F", "#0074D9", "#7FDBFF", "#39CCCC"),
    c("#014421", "#2E8B57", "#66CDAA", "#8FBC8F"),
    c("#3D2C8D", "#5C4D99", "#7D6BBA", "#A89CC8", "#C6BCE4"),
    c("#FFDAB9", "#FFE4B5", "#FFDEAD", "#F5DEB3"),
    c("#D8E2DC", "#A4C3B2", "#6B9080", "#2E4C3B")
  )
)
