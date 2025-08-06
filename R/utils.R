#' Internal: Validate palette length filters
#'
#' Checks whether the supplied palette length constraint (`length`, `min_length`,
#' or `max_length`) is valid: must be a single numeric, non-negative value, and
#' must match at least one palette in `aop_palettes` based on the given comparator.
#' Issues informative errors for invalid inputs, and a warning for unusually large values.
#'
#' @param palettes A tibble of palettes (typically `aop_palettes`) containing a `length` column.
#' @param value A single numeric value to validate.
#' @param comparator One of `"=="`, `">="`, or `"<="`. Determines how to match `value` to palette lengths.
#' @param var_name Name of the parameter being validated (e.g., `"length"`, `"min_length"`), used in error messages.
#'
#' @return Invisibly returns `TRUE` if the input passes all checks. Otherwise throws an error.
#' @keywords internal

validate_palette_length <- function(
  palettes,
  value,
  comparator = c("==", ">=", "<="),
  var_name = "length"
) {
  comparator <- match.arg(comparator)

  # ---- Type & range check ----
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort("`{var_name}` must be a single numeric value.")
  }

  if (value < 0) {
    cli::cli_abort("`{var_name}` must be non-negative.")
  }

  # ---- Warnings (shown regardless of match) ----
  if (value > 30) {
    cli::cli_warn("Unusually large value for `{var_name}`: {value}")
  }

  # ---- Check whether matching palettes exist ----
  length_vec <- palettes$length
  has_match <- switch(
    comparator,
    "==" = any(length_vec == value),
    ">=" = any(length_vec >= value),
    "<=" = any(length_vec <= value)
  )

  if (!has_match) {
    available <- sort(unique(length_vec))
    cli::cli_abort(
      c(
        "No palettes found with {var_name} {comparator} {value}.",
        "i" = "Available lengths: {paste(available, collapse = ', ')}"
      )
    )
  }

  invisible(TRUE)
}
