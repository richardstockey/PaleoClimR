#' Generate a Matrix from a NetCDF File
#'
#' This function reads a NetCDF file and generates a matrix based on the specified parameters.
#'
#' @param map Character. Path to the NetCDF file.
#' @param min.value Numeric. Minimum value for the matrix. Default is -6000.
#' @param max.value Numeric. Maximum value for the matrix. Default is 6000.
#' @param intervals Numeric. Intervals for the matrix. Default is 2000.
#' @param continents.outlined Logical. Whether to outline continents. No default value provided.
#' @param scale.label Character. Label for the scale. No default value provided.
#' @param res Character. Resolution of the NetCDF file. Options are "deg" for degrees or "6min" for 6 minutes. Default is "deg".
#' @param scale Character. Color scale to use. Default is "viridis".
#' @param projection Character. Projection to use for the matrix. Default is 'ESRI:54012'.
#'
#' @return A matrix of values extracted from the NetCDF file.
#'
#' @details
#' This function supports different projections and resolutions for the NetCDF file. It extracts latitude and longitude
#' values and computes edges for these values. The main variable extracted from the NetCDF file is "z", which represents
#' elevation in meters.
#'
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#' @export
scotese.matrix <- function(map, # netcdf file
       min.value = -6000,
       max.value = 6000,
       intervals = 2000,
       continents.outlined,
       scale.label,
       res = "deg",
       scale = "viridis",
       projection = 'ESRI:54012'){

  # Load necessary library
  library(RNetCDF)

  # Open the NetCDF file
  nc <- open.nc(paste0(map))

  # Check the resolution and extract latitude and longitude accordingly
  if(res == "deg"){
  # Extract latitude values (units: degrees)
  lat <- var.get.nc(nc, "lat")
  # Compute latitude edges for the grid
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) 
  # Extract longitude values (units: degrees)
  lon <- var.get.nc(nc, "lon")
  # Compute longitude edges for the grid
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) 
  }
  
  if(res == "6min"){
  # Extract latitude values (units: degrees)
  lat <- var.get.nc(nc, "latitude")
  # Compute latitude edges for the grid
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) 
  # Extract longitude values (units: degrees)
  lon <- var.get.nc(nc, "longitude")
  # Compute longitude edges for the grid
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) 
  }

  # Extract the main variable "z" which represents elevation (units: meters)
  z <- var.get.nc(nc, "z")
  
  # Return the extracted matrix
  return(z)
}
