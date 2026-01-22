#' Extract Grid Data from HADCM3L NetCDF Files
#'
#' This function extracts grid data from imported NetCDF (.nc) files, specifically designed for use with HADCM3L model outputs.
#' It handles both 2D and 3D grid data based on the specified dimensions.
#'
#' @param file Character. The file name (without extension) to read from the specified experiment directory.
#' @param experiment Character. The directory path where the NetCDF file is located.
#' @param dims Integer. The dimensions of the NetCDF data being read in.
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
#' @details The function adjusts longitude values to be within -180 to 180 and reports if any values were outside this range.
#'
#' @importFrom RNetCDF open.nc var.get.nc
#' @export

HADCM3.grid <- function(file = NULL,
                        experiment = NULL,
                        dims = NULL){

  # Open the NetCDF file using the specified experiment path and file name
  nc <- RNetCDF::open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude values from the NetCDF file
  lat <- RNetCDF::var.get.nc(nc, "latitude") # units: degrees north

  # Calculate latitude edges for grid cells
  # The edges are calculated by subtracting half the mean difference between latitudes from each latitude value
  # and adding half the mean difference to the last latitude value
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2))

  # Extract longitude values from the NetCDF file
  lon <- RNetCDF::var.get.nc(nc, "longitude") # units: degrees east

  # Calculate longitude edges for grid cells
  # The edges are calculated similarly to latitude edges
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2))

  # If the data is 3-dimensional, extract depth values
  if(dims == 3){
    depth <- RNetCDF::var.get.nc(nc, "depth_1") # units: metres

    # Calculate depth edges for grid cells
    # The edges are calculated by adding 0 at the start and an approximated value at the end
    depth.edges <- c(0, RNetCDF::var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres
  }

  # ---- wrap longitudes to [-180, 180] and report if out-of-range ----
  wrap_lon <- function(x) ((x + 180) %% 360) - 180
  if (any(lon < -180 | lon > 180)) {
    message("Longitude values outside [-180, 180] detected. Wrapping them.")
    lon <- wrap_lon(lon)
  }
  if (any(lon.edges < -180 | lon.edges > 180)) {
    message("Longitude edges outside [-180, 180] detected. Wrapping them.")
    lon.edges <- wrap_lon(lon.edges)
  }

  # Return grid list
  grid <- if (dims == 3) {
    list(lat = lat, lat.edges = lat.edges,
         lon = lon, lon.edges = lon.edges,
         depth = depth, depth.edges = depth.edges)
  } else {
    list(lat = lat, lat.edges = lat.edges,
         lon = lon, lon.edges = lon.edges)
  }

  # Return the grid data as a list
  return(grid)
  }


