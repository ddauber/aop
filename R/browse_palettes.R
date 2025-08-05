#' Browse and filter Anatomy of Plots colour palettes
#'
#' Provides a visual overview of palettes stored in `aop_palettes`.
#' Users can filter palettes by name, mode, mood, tags or length and
#' optionally sort the results. The output can be returned as a plot,
#' a tibble, or both.
#'
#' @param name Optional character string to partially match palette names.
#' @param mode Optional character vector restricting palettes to the given
#'   background mode(s): "light", "dark", or "both".
#' @param mood Optional character vector of moods to include.
#' @param tags Optional character vector of tags. Only palettes containing
#'   all supplied tags are returned.
#' @param length Optional integer. If supplied, only palettes with exactly
#'   this number of colours are returned.
#' @param min_length Optional integer giving the minimum number of colours
#'   a palette must contain to be returned.
#' @param sort_by Variable to sort by. One of "name", "length", or "mood".
#' @param descending Logical; if `TRUE` sort in descending order.
#' @param ncol,nrow Optional layout settings passed to `ggplot2::facet_wrap`
#'   when a plot is produced.
#' @param dark_mode Logical. If `TRUE`, uses a dark background for the plot.
#' @param return One of "plot", "tibble", or "both" indicating the desired
#'   output type.
#' @param random Logical; if `TRUE`, randomises the order of palettes after
#'   filtering.
#'
#' @return A ggplot object, a tibble, or a list containing both depending on
#'   the value of `return`.
#' @export
#'
#' @examples
#' browse_palettes(mode = "dark", tags = c("storm"))
browse_palettes <- function(
  name = NULL,
  mode = NULL,
  mood = NULL,
  tags = NULL,
  length = NULL,
  min_length = NULL,
  sort_by = c("name", "length", "mood"),
  descending = FALSE,
  ncol = NULL,
  nrow = NULL,
  dark_mode = FALSE,
  return = c("plot", "tibble", "both"),
  random = FALSE
) {
  palettes <- aop_palettes

  # Filtering ---------------------------------------------------------------
  if (!is.null(name)) {
    name <- as.character(name)[1]

    palettes <- dplyr::filter(
      palettes,
      stringr::str_detect(.data$name, stringr::regex(name, ignore_case = TRUE))
    )
  }
  if (!is.null(mode)) {
    palettes <- dplyr::filter(palettes, .data$mode %in% mode)
  }

  if (!is.null(mood)) {
    palettes <- dplyr::filter(palettes, .data$mood %in% mood)
  }

  if (!is.null(tags)) {
    palettes <- dplyr::filter(
      palettes,
      purrr::map_lgl(.data$tags, ~ all(tags %in% .x))
    )
  }

  if (!is.null(length)) {
    palettes <- dplyr::filter(palettes, .data$length == length)
  }

  if (!is.null(min_length)) {
    palettes <- dplyr::filter(palettes, .data$length >= min_length)
  }

  # Sorting ----------------------------------------------------------------
  sort_by <- match.arg(sort_by)
  if (random) {
    palettes <- palettes[sample(nrow(palettes)), ]
  } else {
    palettes <- palettes[order(palettes[[sort_by]], decreasing = descending), ]
  }

  result_tbl <- palettes

  out_type <- match.arg(return)
  if (out_type %in% c("plot", "both")) {
    df <- purrr::pmap_dfr(
      list(palettes$name, palettes$hex, palettes$length, palettes$mode),
      function(name, hex, length, mode) {
        tibble::tibble(
          label = paste0(name, " (", length, ", ", mode, ")"),
          hex = hex,
          index = seq_along(hex)
        )
      }
    )

    bg_colour <- if (dark_mode) "#111111" else "#FFFFFF"
    text_colour <- if (dark_mode) "#eaeaea" else "#333333"

    plt <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = 1, fill = hex)) +
      ggplot2::geom_tile(width = 0.9, height = 0.9) +
      ggplot2::scale_fill_identity() +
      ggplot2::facet_wrap(~label, ncol = ncol, nrow = nrow) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_void() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(color = text_colour, face = "bold"),
        plot.background = ggplot2::element_rect(fill = bg_colour, color = NA),
        panel.background = ggplot2::element_rect(fill = bg_colour, color = NA)
      )
  }

  if (out_type == "plot") {
    return(plt)
  } else if (out_type == "tibble") {
    return(result_tbl)
  } else {
    return(list(plot = plt, tibble = result_tbl))
  }
}

# Visible binding for global variables.
utils::globalVariables(c("hex", "label", "index"))
