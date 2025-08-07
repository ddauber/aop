#' Render AoP Colour Palettes as an HTML Grid
#'
#' Creates a browsable HTML layout showing all colour palettes in a structured and scrollable format.
#' Each palette appears in a separate row, with the palette name on the left and its swatches displayed
#' as rectangular blocks. Each swatch includes the hex code printed below it for easy reference.
#'
#' This function is useful for previewing all palettes visually, sharing them in documentation, or exporting
#' them as a browsable HTML page. It is designed to work with the `aop_palettes` tibble but can be used with
#' any compatible tibble of the same structure.
#'
#' @param palettes A tibble containing palette metadata, typically `aop:::aop_palettes`. The tibble must include
#'   a `name` column (character) and a `hex` column (list of character vectors with hex codes).
#'
#' @return A `htmltools::browsable()` object rendering the palette grid. This is typically displayed in the RStudio Viewer
#'   or can be embedded in a Quarto/HTML document.
#'
#' @examples
#' browse_full_palette()
#'
#' @export
browse_full_palette <- function(palettes = aop:::aop_palettes) {
  # Create one HTML block per palette
  palette_html <- purrr::map(seq_len(nrow(palettes)), function(i) {
    name <- palettes$name[[i]]
    hex <- palettes$hex[[i]]

    # Individual colour swatches with hex code
    swatches <- purrr::map(hex, function(color) {
      htmltools::div(
        style = paste(
          "display: flex;",
          "flex-direction: column;",
          "align-items: center;",
          "margin: 3px;"
        ),
        # Rectangle swatch
        htmltools::div(
          style = paste(
            "width: 80px;",
            "height: 40px;",
            "background-color:",
            color,
            ";",
            "border-radius: 4px;"
          )
        ),
        # Hex code label underneath
        htmltools::div(
          style = paste(
            "font-family: sans-serif;",
            "font-size: 12px;",
            "font-weight: 200;",
            "color: #545963;",
            "margin-top: 4px;"
          ),
          color
        )
      )
    })

    # Combine palette name + swatches row
    htmltools::div(
      style = paste(
        "display: flex;",
        "align-items: flex-start;",
        "margin-bottom: 30px;"
      ),
      # Name column (fixed width)
      htmltools::div(
        style = paste(
          "min-width: 200px;",
          "max-width: 200px;",
          "white-space: nowrap;",
          "overflow: hidden;",
          "text-overflow: ellipsis;",
          "font-family: sans-serif;",
          "font-size: 14px;",
          "font-weight: 400;",
          "color: #292C32;",
          "margin-right: 16px;"
        ),
        name
      ),
      # Swatch strip (flex container)
      htmltools::div(
        style = paste(
          "display: flex;",
          "gap: 0px;",
          "flex-wrap: wrap;"
        ),
        swatches
      )
    )
  })

  # Wrap everything with padding and a title
  htmltools::browsable(
    htmltools::div(
      style = paste(
        "padding: 50px;", # Adds space around entire container
        "font-family: sans-serif;"
      ),
      # Title at the top
      htmltools::tags$h2(
        "AoP Colour Palettes",
        style = paste(
          "margin-bottom: 50px;",
          "color: #292C32;",
          "font-size: 22px;",
          "font-weight: bold;"
        )
      ),
      # All the palette rows
      htmltools::tagList(palette_html)
    )
  )
}
