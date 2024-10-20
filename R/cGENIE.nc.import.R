#' Import Data from cGENIE NetCDF Files
#'
#' This function imports specified variables from cGENIE NetCDF files and extracts relevant grid information.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file.
#' @param experiment A character string indicating the path to the cGENIE experiment directory.
#' @param dims An integer specifying the dimensions of the NetCDF file to read. Default is set to 3 for 3D data.
#' @param model A character string indicating the model type (default is "biogem"). Currently, only "biogem" is supported.
#' @return A list containing latitude, longitude, depth, time, and the specified variable data from the NetCDF file.
#' @details The function reads a NetCDF file based on the specified experiment and model, extracting latitude, longitude,
#' depth, time, and the requested variable. It supports both 2D and 3D datasets.
#'
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @export
#' @examples
#' # Example usage:
#' nc_data <- cGENIE.nc.import("temperature", "experiment_directory", dims = 3)

cGENIE.nc.import <- function(var, experiment, dims = 3, model = "biogem") {

  # Load required libraries
  library(RNetCDF)  # For working with NetCDF files
  library(dplyr)    # For data manipulation (pipe operator)

  # Define the prefix for the NetCDF file based on the model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat")          # units: degrees north
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon")          # units: degrees east
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt")         # units: metres
  depth.edges <- var.get.nc(nc, "zt_edges") # units: metres
  time <- var.get.nc(nc, "time")        # units: year mid-point

  # Extract the specified variable
  var.arr <- var.get.nc(nc, var)

  # Create a summary list with the extracted data
  nc.sum <- list(lat = lat, lon = lon, depth = depth, time = time, var = var.arr)

  # Return the summary list
  return(nc.sum)
}
