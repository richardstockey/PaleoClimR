#' Prints the years that are saved in a cGENIE model experiment
#'
#' @param experiment Directory containing the experiment's netCDF files.
#' @param model The model type; defaults to "biogem".
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @importFrom RNetCDF open.nc var.get.nc
#' @export

cGENIE.years <- function(experiment,
                        model = "biogem",
                        dims = 3){

  # note that dims are essentially arbitrary here
  # anticipating any model with biogem will include both 2d and 3d netcdf output
  # could change but not sure why you would.

  # Define the prefix for the NetCDF file based on the model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  } else {
    stop("Currently only 'biogem' model is supported.")
  }

  # Open the netCDF file based on the dimensions
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  years <- RNetCDF::var.get.nc(nc, "time")

  return(years)
}
