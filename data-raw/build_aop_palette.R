#' Build aop_palette_meta.R from CSV
#'
#' This script reads `aop_palette_db.csv` and outputs `R/aop_palette_meta.R`
#' with a tibble containing all metadata, including a parsed column of hex colours.
#' This approach was chosen to enable easier editing upfront.
#' CONSIDERATION: In the future it might be worth expanding to

library(readr)
library(dplyr)
library(stringr)

# Read the CSV file ----
meta <- read_csv("data-raw/aop_palette_db.csv", show_col_types = FALSE) |>
  mutate(
    hex = str_split(hex, "\\|") |> purrr::map(~ toupper(.x)),
    tags = str_split(tags, ",\\s*")
  )

# EXPORT FUNCTION: EXPORTING THE FILE AFTER DATA CLEANING ----
export_palette_meta <- function(meta, path = "data-raw/aop_palette_db.csv") {
  meta_clean <- meta |>
    mutate(
      hex = purrr::map_chr(hex, ~ paste(toupper(.x), collapse = "|")),
      tags = purrr::map_chr(tags, ~ paste(.x, collapse = ", "))
    )

  readr::write_csv(meta_clean, path)
  message("✅ Palette metadata exported to CSV.")
}

# Create the R file output ----
r_code <- c(
  "#' Colour palettes for Anatomy of Plots",
  "#'",
  "#' This tibble contains palette name, mode, mood, tags, and hex codes.",
  "#' Each palette is a named character vector of hex codes.",
  "#'",
  "#' @format A tibble with one row per palette and the following columns:",
  "#' \\describe{",
  "#'   \\item{id}{Unique numeric ID for the palette}",
  "#'   \\item{name}{Name of the palette (e.g. \"sunset\")}",
  "#'   \\item{length}{Number of colours in the palette}",
  "#'   \\item{mode}{Best suited background: \"dark\", \"light\", or \"both\"}",
  "#'   \\item{mood}{Subjective aesthetic theme, e.g. \"warm\", \"cool\", \"vibrant\"}",
  "#'   \\item{tags}{Character vector of keywords for filtering (e.g. \"gradient\", \"space\")}",
  "#'   \\item{hex}{Character vector of hex colour codes}",
  "#' }",
  "#' @keywords internal",
  "aop_palettes <- tibble::tibble(",
  paste0("  id = c(", paste(meta$id, collapse = ", "), "),"),
  paste0("  name = c(", paste0('"', meta$name, '"', collapse = ", "), "),"),
  paste0("  length = c(", paste(meta$length, collapse = ", "), "),"),
  paste0("  mode = c(", paste0('"', meta$mode, '"', collapse = ", "), "),"),
  paste0("  mood = c(", paste0('"', meta$mood, '"', collapse = ", "), "),"),
  paste0(
    "  tags = list(",
    paste(
      purrr::map_chr(
        meta$tags,
        ~ paste0('c("', paste(.x, collapse = '", "'), '")')
      ),
      collapse = ", "
    ),
    "),"
  ),
  paste0(
    "  hex = list(",
    paste(
      purrr::map_chr(
        meta$hex,
        ~ paste0('c("', paste(.x, collapse = '", "'), '")')
      ),
      collapse = ", "
    ),
    ")"
  ),
  ")"
)

# Write to R file ----
writeLines(r_code, "R/aop_palettes.R")

# Export as new .csv file ----
export_palette_meta(meta)


message("✅ R/aop_palette_meta.R generated successfully.")
