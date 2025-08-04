#' Visualize palette and its most distinct subset
#'
#' @param palette A palette name or a character vector of hex colour codes. Must contain valid hex colour codes.
#' @param k Number of colours to select. Must be a single positive integer not exceeding the length of `palette`.
#' @param title Optional plot title
#' @param dark_mode Logical. If `TRUE`, uses a dark background for the plot. Default is `FALSE`.
#'
#' @return A ggplot comparing full vs selected palette
#' @details Validates inputs and throws informative errors when conditions are not met. If `palette` is a single
#' string, it is treated as a palette name and looked up via `aop_palette()`.
#' @export

test_palette_selection <- function(
  palette,
  k = 2,
  title = NULL,
  dark_mode = FALSE
) {
  if (!is.character(palette)) {
    rlang::abort("`palette` must be a character vector.")
  }
  if (length(palette) == 1 && !grepl("^#[0-9A-Fa-f]{6}$", palette)) {
    palette_name <- palette
    palette <- tryCatch(
      aop_palette(palette_name),
      error = function(e) {
        rlang::abort(sprintf("Palette '%s' not found.", palette_name))
      }
    )
  }
  if (any(!grepl("^#[0-9A-Fa-f]{6}$", palette))) {
    rlang::abort("`palette` must contain valid hex colour codes.")
  }
  if (
    length(k) != 1 || !is.numeric(k) || is.na(k) || k <= 0 || k != as.integer(k)
  ) {
    rlang::abort("`k` must be a single positive integer.")
  }
  if (k > length(palette)) {
    rlang::abort("`k` must not exceed the length of `palette`.")
  }
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

# Visible binding for global variables.
utils::globalVariables(c("colour", "group", "index"))
