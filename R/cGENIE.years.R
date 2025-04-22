#' Prints the years that are saved in a cGENIE model experiment
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param model The model type; defaults to "biogem".
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @importFrom RNetCDF open.nc var.get.nc
#' @export

cGENIE.years <- function(var, experiment,
                        model = "biogem"){

  # Load necessary libraries
  RNetCDF::library(RNetCDF)

  # Define the prefix based on the selected model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the netCDF file based on the dimensions
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  years <- RNetCDF::var.get.nc(nc, "time")

  print(years)

  return(years)
}
