#' Extract Grid Data from HADCM3L NetCDF Files
#'
#' This function extracts grid data from imported NetCDF (.nc) files, specifically designed for use with HADCM3 model outputs.
#'
#' @param var Character. The variable name to extract from the NetCDF file.
#' @param file Character. The file name (without extension) to read from the specified experiment directory.
#' @param experiment Character. The directory path where the NetCDF file is located.
#' @param dims Integer. The dimensions of the NetCDF data being read in. Set to 3 for 3D data (default) or 2 for 2D data.
#'
#' @return A list containing the grid data:
#' \itemize{
#'   \item \code{lat} - Latitude values (degrees north).
#'   \item \code{lat.edges} - Latitude edges for grid cells.
#'   \item \code{lon} - Longitude values (degrees east).
#'   \item \code{lon.edges} - Longitude edges for grid cells.
#'   \item \code{depth} - (Only if \code{dims} is 3) Depth values (meters).
#'   \item \code{depth.edges} - (Only if \code{dims} is 3) Depth edges for grid cells.
#' }
#'
#' @details
#' This function reads in NetCDF files using the \code{RNetCDF} package and extracts latitude, longitude, and optionally depth data.
#' The function adjusts the longitude values to be within the range of -180 to 180 degrees if necessary.
#' The grid data is returned as a list with named elements for easy access.
#'
#' @note
#' The latitude and longitude values are adjusted to remove any values outside the realistic range (-90 to 90 for latitude and -180 to 180 for longitude).
#' Depth values are only included if \code{dims} is set to 3.
#'
#' @examples
#' \dontrun{
#' grid_data <- HADCM3.grid(var = "insitu_T_ym_dpth", file = "o.pgclann", experiment = "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444", dims = 3)
#' }
#'
#' @import RNetCDF dplyr sf sp ggspatial reshape2 ggplot2 pals viridis
#' @export

HADCM3.grid <- function(var, file, experiment, dims){


  # This script reads and processes NetCDF data for climate projections using the HADCM3 model.
  # It includes various libraries for data manipulation, spatial data handling, and visualization.

  # Libraries:
  # - RNetCDF: Provides functions to read and write NetCDF files.
  # - dplyr: A grammar of data manipulation, providing a consistent set of verbs to help you solve the most common data manipulation challenges.
  # - sf: Simple features for R, which provides support for spatial vector data.
  # - sp: Classes and methods for spatial data.
  # - ggspatial: Spatial data visualizations with ggplot2.
  # - reshape2: Flexibly reshape data.
  # - ggplot2: A system for declaratively creating graphics, based on The Grammar of Graphics.
  # - pals: Provides color palettes for data visualization.
  # - viridis: Colorblind-friendly color maps for R.

  # Notes:
  # - Other projection options include EPSG:6933 (Lambert Cylindrical Equal Area).
  # - The dimensions of the NetCDF file are set to 3D by default.
  # - The palette_name parameter should be followed by a number (e.g., parula(1000)).
  # - Alternative color palettes can be found at https://r-charts.com/color-palettes/.

  # Example usage:
  # - Set the experiment path and file name.
  # - Specify the variable to be read from the NetCDF file.
  # - Open the NetCDF file using the open.nc function from the RNetCDF library.
  # Load necessary libraries for data manipulation and NetCDF file handling
  library(RNetCDF)   # Provides functions to read and write NetCDF files
  library(dplyr)     # A grammar of data manipulation, providing a consistent set of verbs to help you solve the most common data manipulation challenges
  # Open the NetCDF file using the specified experiment path and file name
  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude values from the NetCDF file
  lat <- var.get.nc(nc, "latitude") # units: degrees north

  # Calculate latitude edges for grid cells
  # The edges are calculated by subtracting half the mean difference between latitudes from each latitude value
  # and adding half the mean difference to the last latitude value
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) 

  # Extract longitude values from the NetCDF file
  lon <- var.get.nc(nc, "longitude") # units: degrees east

  # Calculate longitude edges for grid cells
  # The edges are calculated similarly to latitude edges
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) 

  # If the data is 3-dimensional, extract depth values
  if(dims == 3){
    depth <- var.get.nc(nc, "depth_1") # units: metres

    # Calculate depth edges for grid cells
    # The edges are calculated by adding 0 at the start and a fudged value at the end
    depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres
  }

  # Adjust longitude values to be within the range of -180 to 180 degrees if necessary
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] - 360
    lon[lon >180] <- lon[lon >180] -360
  }

  # Filter latitude values to ensure they are within the realistic range (-90 to 90 degrees)
  lat <- lat[lat < 90 & lat > -90]

  # Filter latitude edges to ensure they are within the realistic range (-90 to 90 degrees)
  lat.edges <- lat.edges[lat.edges < 90 & lat.edges > -90]

  # Filter longitude values to ensure they are within the realistic range (-180 to 180 degrees)
  lon <- lon[lon < 180 & lon > -180]

  # Filter longitude edges to ensure they are within the realistic range (-180 to 180 degrees)
  lon.edges <- lon.edges[lon.edges < 180 & lon.edges > -180]

  # If the data is 3-dimensional, create a list containing latitude, longitude, and depth information
  if (dims == 3) {
    grid <- list(
      lat,        # Latitude values
      lat.edges,  # Latitude edges for grid cells
      lon,        # Longitude values
      lon.edges,  # Longitude edges for grid cells
      depth,      # Depth values
      depth.edges # Depth edges for grid cells
    )
    
    # Assign names to the elements of the list for easy access
    names(grid) <- c(
      "lat",        # Latitude values
      "lat.edges",  # Latitude edges for grid cells
      "lon",        # Longitude values
      "lon.edges",  # Longitude edges for grid cells
      "depth",      # Depth values
      "depth.edges" # Depth edges for grid cells
    )
  }

  # If the data is 2-dimensional, create a list containing only latitude and longitude information
  if (dims == 2) {
    grid <- list(
      lat,        # Latitude values
      lon,        # Longitude values
      lat.edges,  # Latitude edges for grid cells
      lon.edges   # Longitude edges for grid cells
    )
    
    # Assign names to the elements of the list for easy access
    names(grid) <- c(
      "lat",        # Latitude values
      "lat.edges",  # Latitude edges for grid cells
      "lon",        # Longitude values
      "lon.edges"   # Longitude edges for grid cells
    )
  }

  # Return the grid data as a list
  return(grid)
  }


