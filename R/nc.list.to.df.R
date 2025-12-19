#' Convert gridded NetCDF array output to a tidy spatial data frame
#'
#' Internal utility function that converts pre-extracted NetCDF variables
#' stored as arrays into a long-format data frame with latitude–longitude
#' midpoints and grid-cell bounds.
#'
#' The function is model-agnostic and should work for any regularly gridded
#' lat–lon NetCDF output. Model-specific exceptions (e.g. certain cGENIE
#' diagnostics with non-standard grids) are handled explicitly where required.
#'
#' @param lat Numeric vector of latitude midpoints.
#' @param lat.edges Numeric vector of latitude cell edges.
#' @param lon Numeric vector of longitude midpoints.
#' @param lon.edges Numeric vector of longitude cell edges.
#' @param var Character string giving the variable name.
#' @param depth.level Integer depth index to extract (3D variables only).
#' @param dims Integer number of spatial dimensions (2 or 3).
#' @param time.step Integer time index to extract.
#'
#' @return A data frame with columns:
#'   \code{lon.mid}, \code{lon.min}, \code{lon.max},
#'   \code{lat.mid}, \code{lat.min}, \code{lat.max},
#'   and \code{var}.
#'
#' @details
#' This function does not read NetCDF files directly. It operates on arrays
#' already extracted from NetCDF output and assumes a regular lat–lon grid.
#' Special cases for particular model diagnostics (e.g. cGENIE streamfunction
#' or topography fields) are handled internally.
#'
#' @keywords internal
#' @importFrom reshape2 melt
nc.list.to.df <- function(lat,
                          lat.edges,
                          lon,
                          lon.edges,
                          var,
                          depth.level,
                          dims,
                          time.step = NULL) {

  # ---- sanity checks ----
  if (!dims %in% c(2, 3)) {
    stop("dims must be 2 or 3")
  }

  has_time <- !is.null(time.step)

  # ---- extract variable slice ----
  if (dims == 3) {

    if (has_time) {
      # lon × lat × depth × time
      slice <- var.arr[, , depth.level, time.step]
    } else {
      # lon × lat × depth
      slice <- var.arr[, , depth.level]
    }

  } else if (dims == 2) {

    if (var == "grid_topo" || var == "phys_psi") {
      # handled separately below
      slice <- var.arr
    } else if (has_time) {
      # lon × lat × time
      slice <- var.arr[, , time.step]
    } else {
      # lon × lat
      slice <- var.arr
    }
  }

  values <- reshape2::melt(slice)$value

  # ---- construct spatial dataframe ----
  if (dims == 3) {

    df <- data.frame(
      lon.mid = rep(lon, times = length(lat)),
      lon.min = rep(lon.edges[-length(lon.edges)], times = length(lat)),
      lon.max = rep(lon.edges[-1], times = length(lat)),
      lat.mid = rep(lat, each = length(lon)),
      lat.min = rep(lat.edges[-length(lat.edges)], each = length(lon)),
      lat.max = rep(lat.edges[-1], each = length(lon)),
      var     = values
    )

  } else if (dims == 2) {

    if (var == "grid_topo") {

      df <- data.frame(
        lon.mid = rep(lon, times = length(lat)),
        lon.min = rep(lon.edges[-length(lon.edges)], times = length(lat)),
        lon.max = rep(lon.edges[-1], times = length(lat)),
        lat.mid = rep(lat, each = length(lon)),
        lat.min = rep(lat.edges[-length(lat.edges)], each = length(lon)),
        lat.max = rep(lat.edges[-1], each = length(lon)),
        var     = values
      )

    } else if (var == "phys_psi") {

      df <- data.frame(
        lon.mid = rep(lon + 5, times = length(lat.edges)),
        lon.min = rep(lon.edges[-length(lon.edges)] + 5, times = length(lat.edges)),
        lon.max = rep(lon.edges[-1] + 5, times = length(lat.edges)),
        lat.mid = rep(lat.edges, each = length(lon)),
        lat.min = rep(lat.edges, each = length(lon)),
        lat.max = rep(lat.edges, each = length(lon)),
        var     = values
      )

    } else {

      df <- data.frame(
        lon.mid = rep(lon, times = length(lat)),
        lon.min = rep(lon.edges[-length(lon.edges)], times = length(lat)),
        lon.max = rep(lon.edges[-1], times = length(lat)),
        lat.mid = rep(lat, each = length(lon)),
        lat.min = rep(lat.edges[-length(lat.edges)], each = length(lon)),
        lat.max = rep(lat.edges[-1], each = length(lon)),
        var     = values
      )
    }
  }

  return(df)
}
