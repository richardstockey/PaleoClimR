# zzz.R --- package globals and imports
#' @keywords internal

# Avoid "no visible binding for global variable" NOTES in R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".data", "geometry")
  )
}

# Explicitly import pipe operator for R CMD check
#' @importFrom magrittr %>%
NULL
