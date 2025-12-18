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
                          time.step){

  # Build data frame
  if (dims == 3) {
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat)),
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat)),
      rep(lon.edges[2:length(lon.edges)], times = length(lat)),
      rep(lat, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges) - 1)], each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], each = length(lon)),
      reshape2::melt(var.arr[, , depth.level, time.step])$value
    ))
    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  } else if (dims == 2) {
    if (var == "grid_topo") { # this is a specific cGENIE use case
      df <- as.data.frame(cbind(
        rep(lon, times = length(lat)),
        rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat)),
        rep(lon.edges[2:length(lon.edges)], times = length(lat)),
        rep(lat, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges) - 1)], each = length(lon)),
        rep(lat.edges[2:length(lat.edges)], each = length(lon)),
        reshape2::melt(var.arr)$value
      ))
      names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    } else if (var == "phys_psi") {  # this is a specific cGENIE use case
      df <- as.data.frame(cbind(
        rep(lon + 5, times = length(lat.edges)),
        rep(lon.edges[1:(length(lon.edges) - 1)] + 5, times = length(lat.edges)),
        rep(lon.edges[2:length(lon.edges)] + 5, times = length(lat.edges)),
        rep(lat.edges, each = length(lon)),
        rep(lat.edges, each = length(lon)),
        rep(lat.edges, each = length(lon)),
        reshape2::melt(var.arr)$value
      ))
      names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    } else {
      df <- as.data.frame(cbind(
        rep(lon, times = length(lat)),
        rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat)),
        rep(lon.edges[2:length(lon.edges)], times = length(lat)),
        rep(lat, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges) - 1)], each = length(lon)),
        rep(lat.edges[2:length(lat.edges)], each = length(lon)),
        reshape2::melt(var.arr[, , time.step])$value
      ))
      names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    }
  }

  return(df)
}
