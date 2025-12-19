#' Import Data from HADCM3 NetCDF Files
#'
#' This function imports specified variables from HADCM3 NetCDF files (as formatted by the BRIDGE Group) and extracts relevant grid information.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file.
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param dims An integer specifying the dimensions of the NetCDF file to read. Default is set to 3 for 3D data but can alternatively be set to 2. .
#' @return A list containing latitude, longitude, depth, time, and the specified variable data from the NetCDF file.
#' @details The function reads a NetCDF file based on the specified experiment and model, extracting latitude, longitude,
#' depth, time, and the requested variable. It supports both 2D and 3D datasets.
#'
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @export
#'
HADCM3.get.nc <- function(var,
                          file,
                          experiment,
                          dims = 3) {


  # Open the NetCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude and longitude variables
  lat <- RNetCDF::var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # Calculate latitude edges for plotting
  lon <- RNetCDF::var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # Calculate longitude edges for plotting

  # Extract depth variable if the data is 3D
  if(dims == 3){
    depth <- RNetCDF::var.get.nc(nc, "depth_1") # units: metres
    depth.edges <- c(0, RNetCDF::var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # Calculate depth edges
  }
  else{
    depth <- NULL
    depth.edges <- NULL
  }

  # Detect whether time variable exists
  nvars <- RNetCDF::file.inq.nc(nc)$nvars

  vars <- vapply(
    seq_len(nvars),
    function(i) RNetCDF::var.inq.nc(nc, i - 1)$name,
    character(1)
  )

  if ("t" %in% vars) {
    time <- RNetCDF::var.get.nc(nc, "t")  # year mid-point
  } else {
    time <- NULL
  }
  # Extract the specified variable
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # Create a summary list with the extracted data
  nc.sum <- list(lat = lat,
                 lat.edges = lat.edges,
                 lon = lon,
                 lon.edges = lon.edges,
                 depth = depth,
                 time = time,
                 var = var.arr)

  # Return the summary list
  return(nc.sum)
}
