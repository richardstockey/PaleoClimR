#' Prints the years that are saved in a cGENIE model experiment
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param model The model type; defaults to "biogem".
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @importFrom RNetCDF open.nc var.get.nc
#' @export

cGENIE.years <- function(experiment,
                        var = "ocn_temp",
                        model = "biogem",
                        dims = 3){

  # note that var and dims are essentially arbitrary here
  # anticipating any model with biogem will include temperature
  # could change but not sure why you would.

  # Define the prefix based on the selected model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the netCDF file based on the dimensions
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  years <- RNetCDF::var.get.nc(nc, "time")

  return(years)
}
