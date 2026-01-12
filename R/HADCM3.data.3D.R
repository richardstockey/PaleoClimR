#' Extract and Process 3D Data from HADCM3 NetCDF Files
#'
#' This function extracts and processes 3D data from HADCM3 model NetCDF files
#' based on latitude, longitude, depth, and time. It converts the 3D array into a
#' 2D dataframe with geographical coordinates and depth levels, making the data
#' easier to visualize or further process.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file.
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param year Year to extract data for (default is "default", meaning the last or only time point (note this is slightly different to cGENIE.data because HADCM3 outputs more commonly have only one time point)).
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
HADCM3.data.3D <- function(var, file, experiment,
                           year = "default") {

  # Set dimensionality to 3D by default
  dims <- 3

  nc.list <- HADCM3.get.nc(var = var,
                           file = file,
                           experiment = experiment,
                           dims = dims
                          )

  # Select time step
  if (year == "default") {
    # if there are multiple timesteps, this will be the last one
    # if there is only one (no) timestep[s] this will be NULL becase then it will be a 'random' number, which is then handled in nc.list.to.df()
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
                           depth.level = depth.level,
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
