#' Extract and Process 3D Data from cGENIE NetCDF Files
#'
#' This function extracts and processes 3D data from cGENIE model NetCDF files
#' based on latitude, longitude, depth, and time. It converts the 3D array into a
#' 2D dataframe with geographical coordinates and depth levels, making the data
#' easier to visualize or further process.
#'
#' @param var A string specifying the variable name to extract from the NetCDF file.
#'            Examples include "ocn_temp", "ocn_O2", etc.
#' @param experiment A string specifying the path to the folder or file prefix
#'                   where the experiment's data is stored.
#' @param year A numeric value or the string "default" indicating the time step
#'             to extract data for. If "default", the latest time step will be selected.
#' @param model A string indicating the model type (default is "biogem").
#'               Currently supports the "biogem" model.
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
#' @details
#' 1. The function assumes a 3D NetCDF file (longitude, latitude, depth) is used.
#' 2. Default projection adjustments are made for handling longitudes that cross
#'    0 degrees.
#' 3. The data is reshaped from a 3D array into a 2D dataframe for easier analysis.
#'
#' @example
#' data <- cGENIE.data.3D(var = "ocn_O2", experiment = "my_experiment", year = "default")
#' This extracts dissolved oxygen data from the "my_experiment" NetCDF file for the final model year.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#'
#' @export
cGENIE.data.3D <- function(var, experiment,
                           year = "default",
                           model = "biogem") {

  # Load necessary libraries
  library(RNetCDF)
  library(dplyr)
  library(reshape2)

  # Define model prefix for file paths
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Set dimensionality to 3D by default
  dims <- 3
  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat")  # Latitude (degrees north)
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon")  # Longitude (degrees east)
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt")  # Depth (meters)
  depth.edges <- var.get.nc(nc, "zt_edges")  # Depth edges (meters)
  time <- var.get.nc(nc, "time")  # Time (year mid-point)

  # Extract the variable array for the chosen time step
  var.arr <- var.get.nc(nc, var)

  # Determine which time step to extract
  if (year == "default") {
    time.step <- length(time)
  } else {
    time.step <- year
  }

  # Adjust grid for longitude projection (to 0-360 degrees if needed)
  if (mean(between(lon, -180, 180)) < 1) {
    lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
    lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Initialize an empty dataframe for all depth levels
  df.sum <- data.frame(lon.mid = double(), lon.min = double(), lon.max = double(),
                       lat.mid = double(), lat.min = double(), lat.max = double(),
                       var = double(), depth.level = double(), depth = double(),
                       lon.range = double())

  # Loop through all depth levels and build a dataframe
  for (depth.level in 1:length(depth)) {
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[, , depth.level, time.step]))$value
    ))

    names(df) <- c("lon.mid", "lon.min", "lon.max",
                   "lat.mid", "lat.min", "lat.max", "var")

    # Add depth information to the dataframe
    df$depth.level <- depth.level
    df$depth <- depth[depth.level]
    df$depth.min <- depth.edges[depth.level]
    df$depth.max <- depth.edges[depth.level + 1]

    # Filter out invalid geographical coordinates
    df <- df %>% filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    # Adjust longitudes that cross map boundaries
    df$lon.range <- abs(df$lon.min - df$lon.max)
    df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
    df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

    # Append the processed dataframe for the current depth level
    df.sum <- rbind(df.sum, df)
  }

  return(df.sum)
}
