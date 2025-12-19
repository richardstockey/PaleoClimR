#' cGENIE Points Data
#'
#' This function matches palaeocoordinates to cGENIE model data.
#'
#' @param var Character. The variable to extract from the cGENIE model.
#' @param experiment Character. The experiment identifier for the cGENIE model.
#' @param depth.level Numeric. The depth level to extract from the cGENIE model. Default is 1.
#' @param dims Numeric. The number of dimensions in the cGENIE model. Default is 3.
#' @param year Year to extract data for (default is "default", meaning the last time point).
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#' @param max_dist Maximum allowed distance for matching (km, default 1000 km).
#' @param na.method How to handle NA climate values: "nearest" (default), "same.lat", "keep".
#'
#' @return A data frame or 3D array with the original coordinates and matched climate data from the cGENIE model.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#' @export

cGENIE.points.data <- function(var = NULL,
                                  experiment = NULL,
                                  depth.level = 1,
                                  dims = 3,
                                  year = "default",
                                  coord.dat, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                                  lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                                  lng.name = "p_lng", # name IF generated from rotated paleoverse coordinates...
                                  max_dist = 1000,
                                  na.method = "nearest"
                               )
{

  clim.dat <- cGENIE.data(var = var,
                          experiment = experiment,
                          depth.level = depth.level,
                          dims = dims,
                          year = year)

  grid.dat <- cGENIE.grid(experiment = experiment,
                          dims = dims)

  coord.dat <- match.climate.coords(clim.dat = clim.dat,
                                     grid.dat = grid.dat,
                                     coord.dat = coord.dat,
                                     lat.name = lat.name,
                                     lng.name = lng.name,
                                     max_dist = max_dist,
                                     na.method = na.method)

  return(coord.dat)
}
