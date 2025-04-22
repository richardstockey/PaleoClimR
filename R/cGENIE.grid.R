#' Extract Grid Data from cGENIE netCDF Files
#'
#' This function extracts grid-related data (latitude, longitude, and depth) from a specified cGENIE netCDF file.
#' It can handle both 2D and 3D grid data based on the specified dimensions.
#'
#' @param experiment \code{character}. A character string specifying the path to the experiment folder where the target cGENIE netCDF files are stored.
#' @param dims \code{numberic}. Number of dimensions in the netCDF file. Defaults to 3.
#' @param model \code{string}. The model type (default is "biogem"). Currently supports the "biogem" model.
#' @return A list containing latitude, longitude, and depth information, along with their respective edges.
#' @details The function handles the adjustment of longitude values to ensure they are within the range of -180 to 180.
#' It returns a list containing the relevant grid data based on the specified dimensions (2D or 3D).
#'
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr between
#' @export

cGENIE.grid <- function(experiment, dims, model = "biogem") {

  # Define the prefix for the NetCDF file based on the model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the NetCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables
  lat <- RNetCDF::var.get.nc(nc, "lat")          # units: degrees north
  lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")
  lon <- RNetCDF::var.get.nc(nc, "lon")          # units: degrees east
  lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")
  depth <- RNetCDF::var.get.nc(nc, "zt")         # units: metres
  depth.edges <- RNetCDF::var.get.nc(nc, "zt_edges") # units: metres
  time <- RNetCDF::var.get.nc(nc, "time")        # units: year mid-point

  # Adjust longitude for projection on 0 degrees
  if (mean(dplyr::between(lon, -180, 180)) < 1) {
    lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
    lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Filter latitude and longitude within valid ranges
  # is this necessary here? Do more testing
  lat <- lat[lat <= 90 & lat >= -90]
  lat.edges <- lat.edges[lat.edges <= 90 & lat.edges >= -90]
  lon <- lon[lon <= 180 & lon >= -180]
  lon.edges <- lon.edges[lon.edges <= 180 & lon.edges >= -180]

  # Create a list to store grid data based on dimensions
  if (dims == 3) {
    grid <- list(
      lat = lat,
      lat.edges = lat.edges,
      lon = lon,
      lon.edges = lon.edges,
      depth = depth,
      depth.edges = depth.edges
    )
  } else if (dims == 2) {
    grid <- list(
      lat = lat,
      lat.edges = lat.edges,
      lon = lon,
      lon.edges = lon.edges
    )
  }

  # Return the grid data
  return(grid)
}
