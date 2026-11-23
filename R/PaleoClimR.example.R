#' Path to an example dataset compatible with PaleoClimR
#'
#' Small exampledata files shipped with PaleoClimR, for use in examples, tests and vignettes.
#'
#' @param name Name of an example experiment folder. If NULL, lists the available examples.
#' @return A path to the example experiment directory, or a character vector of available names.
#' @examples
#' PaleoClimR.example()
#' PaleoClimR.example("cGENIE.permian.example")
#' @export
PaleoClimR.example <- function(name = NULL) {
  if (is.null(name)) return(list.files(system.file("extdata", package = "PaleoClimR")))
  system.file("extdata", name, package = "PaleoClimR", mustWork = TRUE)
}
