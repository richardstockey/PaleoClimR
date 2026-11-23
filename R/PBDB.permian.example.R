#' Example Permian occurrence data
#'
#' A sample of Paleobiology Database occurrence records prepared for use with
#' PaleoClimR's point-matching and mapping functions.
#'
#' @format A data frame with 13,772 rows and 150 columns. It includes
#'   occurrence information and geographic coordinates, including `p_lng` and
#'   `p_lat` for palaeogeographic longitude and latitude.
#' @source Changhsingian occurrence records downloaded from the
#'   [Paleobiology Database](https://paleobiodb.org) on 28 October 2024.
#'   Data were cleaned by Alison T. Cribb and Richard G. Stockey following
#'   the recommendations of Flannery-Sutherland et al. (2022), then
#'   palaeorotated to the middle age of the Changhsingian using
#'   `palaeoverse::palaeorotate()` (Jones et al., 2023) following
#'   the PALEOMAP reconstruction.
#' @references
#' Flannery-Sutherland, J. T., Raja, N. B., Kocsis, Á. T., & Kiessling, W.
#' (2022). fossilbrush: An R package for automated detection and resolution
#' of anomalies in palaeontological occurrence data. *Methods in Ecology and
#' Evolution*, 13(11), 2404–2418. \doi{10.1111/2041-210X.13966}.
#'
#' Jones, L. A., Gearty, W., Allen, B. J., Eichenseer, K., Dean, C. D.,
#' Galván, S., Kouvari, M., Godoy, P. L., Nicholl, C. S. C., Buffan, L.,
#' Dillon, E. M., Flannery-Sutherland, J. T., & Chiarenza, A. A. (2023).
#' palaeoverse: A community-driven R package to support palaeobiological
#' analysis. *Methods in Ecology and Evolution*, 14(9), 2205–2215.
#' \doi{10.1111/2041-210X.14099}.
#' @usage data(PBDB.permian.example)
#' @docType data
#' @name PBDB.permian.example
NULL
