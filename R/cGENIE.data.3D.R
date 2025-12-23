#' Extract and Process 3D Data from cGENIE NetCDF Files
#'
#' This function extracts and processes 3D data from cGENIE model NetCDF files
#' based on latitude, longitude, depth, and time. It converts the 3D array into a
#' 2D dataframe with geographical coordinates and depth levels, making the data
#' easier to visualize or further process.
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param dims Number of dimensions in the netCDF file. Defaults to 3.
#' @param year Year to extract data for (default is "default", meaning the last time point).
#' @param model The model type; defaults to "biogem".
#'
#' @return A dataframe (`df.sum`) containing:
#'   - `lon.mid`: Midpoint of the longitude.
#'   - `lon.min`: Minimum longitude boundary.
#'   - `lon.max`: Maximum longitude boundary.
#'   - `lat.mid`: Midpoint of the latitude.
#'   - `lat.min`: Minimum latitude boundary.
#'   - `lat.max`: Maximum latitude boundary.
#'   - `var`: Extracted variable values (e.g., temperature, salinity).
#'   - `depth.level`: Depth level index (1 for surface, increasing with depth).
#'   - `depth`: Actual depth value in meters.
#'   - `depth.min`: Minimum depth boundary.
#'   - `depth.max`: Maximum depth boundary.
#'   - `lon.range`: Range of longitude for handling map boundaries.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#'
#' @export
cGENIE.data.3D <- function(var, experiment,
                           year = "default",
                           model = "biogem") {

  # Set dimensionality to 3D by default
  dims <- 3

  nc.list <- cGENIE.get.nc(var = var,
                           experiment = experiment,
                           dims = dims,
                           model = model)

  # Select time step
  if (year == "default") {
    time.step <- dim(nc.list$time)[1]
  } else {
    time.step <- which(nc.list$time == year)
  }

  # Initialize an empty dataframe for all depth levels
  nc.df.depth.sum <- data.frame(lon.mid = double(), lon.min = double(), lon.max = double(),
                       lat.mid = double(), lat.min = double(), lat.max = double(),
                       var = double(), depth.level = double(), depth = double())

  # Loop through all depth levels and build a dataframe
  for (depth.level in seq_along(nc.list$depth)) {
    nc.df.depth <- nc.list.to.df(lat = nc.list$lat,
                           lat.edges = nc.list$lat.edges,
                           lon = nc.list$lon,
                           lon.edges = nc.list$lon.edges,
                           var.arr = nc.list$var,
                           depth = depth.level,
                           dims = dims,
                           time.step = time.step
    )
    # Add depth information to the dataframe
    nc.df.depth$depth.level <- depth.level
    nc.df.depth$depth <- nc.list$depth[depth.level]
    nc.df.depth$depth.min <- nc.list$depth.edges[depth.level]
    nc.df.depth$depth.max <- nc.list$depth.edges[depth.level + 1]

    # Append the processed dataframe for the current depth level
    nc.df.depth.sum <- rbind(nc.df.depth.sum, nc.df.depth)
  }

  return(nc.df.depth.sum)
}
