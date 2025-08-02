#' Visualize palette and its most distinct subset
#'
#' @param palette A character vector of hex colour codes
#' @param k Number of colours to select
#' @param title Optional plot title
#' @param dark_mode Logical. If `TRUE`, uses a dark background for the plot. Default is `FALSE`.
#'
#' @return A ggplot comparing full vs selected palette
#' @export
test_palette_selection <- function(
  palette,
  k = 2,
  title = NULL,
  dark_mode = FALSE
) {
  stopifnot(is.character(palette), k >= 1, k <= length(palette))

  selected <- select_distinct_colours(palette, k)

  df <- dplyr::tibble(
    group = c(rep("Full palette", length(palette)), rep("Selected", k)),
    colour = c(palette, selected),
    index = c(seq_along(palette), seq_along(selected))
  )

  bg_colour <- if (dark_mode) "#111111" else "#FFFFFF"
  text_colour <- if (dark_mode) "#eaeaea" else "#333333"

  ggplot2::ggplot(df, ggplot2::aes(x = index, y = group, fill = colour)) +
    ggplot2::geom_tile(width = 0.9, height = 0.9) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_discrete(limits = c("Selected", "Full palette")) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = bg_colour, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_colour, color = NA),
      plot.title = ggplot2::element_text(color = text_colour, face = "bold")
    ) +
    ggplot2::ggtitle(title %||% "Colour selection")
}
