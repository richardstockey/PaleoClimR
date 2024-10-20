#' Summarize Bottom-Water Redox Conditions
#'
#' This function processes output from a cGENIE experiment to summarize the redox conditions
#' of bottom-water environments. It categorizes the ocean bottom water into anoxic, suboxic, and oxic conditions
#' based on specified oxygen thresholds. The function also handles the topographic grid data to ensure correct depth assignments.
#'
#' @param experiment A character string specifying the path to the experiment folder, where the necessary NetCDF files are stored.
#' @param anox.thresh Numeric. The oxygen threshold (in mol/kg) for classifying anoxic bottom waters. Default is 0.
#' @param subox.thresh Numeric. The oxygen threshold (in mol/kg) for classifying suboxic bottom waters. Default is 4.8e-6 (based on Sperling et al. 2015).
#'
#' @details This function reads the NetCDF output of a cGENIE experiment, processes the ocean bottom-water oxygen data (`ocn_O2`), and
#' categorizes each bottom-water grid cell as anoxic, suboxic, or oxic. The topographic data (`grid_topo`) is used to align the depth of each grid cell with the
#' correct ocean layer, and the function outputs the fraction of anoxic, suboxic, and oxic bottom waters.
#'
#' The redox classification thresholds are based on oxygen concentration values, with:
#' - Anoxic: oxygen concentration \eqn{\leq} anox.thresh
#' - Suboxic: oxygen concentration between anox.thresh and subox.thresh
#' - Oxic: oxygen concentration \eqn{>} subox.thresh
#'
#' @return A list with the following components:
#' \describe{
#'   \item{f.anox}{Fraction of bottom-water grid cells classified as anoxic.}
#'   \item{f.subox}{Fraction of bottom-water grid cells classified as suboxic.}
#'   \item{f.ox}{Fraction of bottom-water grid cells classified as oxic.}
#'   \item{bw.array}{A matrix of bottom-water oxygen concentrations for each grid cell.}
#' }
#'
#' @references Sperling, E. A., et al. 2015 The Ecological Physiology of Earth's Second Oxygen Revolution.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- cGENIE.bw.redox("path_to_experiment")
#' print(result$f.anox)
#' print(result$f.subox)
#' print(result$f.ox)
#' }
#'
#' @import RNetCDF
#' @import dplyr
#' @export

cGENIE.bw.redox <- function(experiment, anox.thresh = 0, subox.thresh = 4.8e-6, lower.anox.lim = TRUE){
  # -------------------------------------------------------------------------------------------------------
  # Parameters:
  # experiment   : A character string indicating the path to the cGENIE experiment directory.
  # anox.thresh  : Numeric, threshold for defining anoxic conditions (default = 0 umol/kg, i.e., no oxygen).
  # subox.thresh : Numeric, threshold for defining suboxic conditions (default = 4.8 umol/kg based on Sperling et al., 2015).
  # lower.anox.lim : Logical, if TRUE, sets a minimum anoxic fraction (f.anox) of 0.1% for plotting purposes (default = TRUE).
  # -------------------------------------------------------------------------------------------------------

  # Load required libraries
  library(RNetCDF)
  library(dplyr)

  # Initialize dimensions for 3D ocean grid and set oxygen variable name
  dims <- 3
  var <- "ocn_O2"

  # Open the NetCDF file from the experiment directory
  nc <- open.nc(paste0(experiment, "/biogem/fields_biogem_", dims, "d", ".nc"))

  # Extract spatial and temporal variables from NetCDF (latitude, longitude, depth, and time)
  lat <- var.get.nc(nc, "lat")               # Latitude (degrees north)
  lat.edges <- var.get.nc(nc, "lat_edges")   # Latitude edges
  lon <- var.get.nc(nc, "lon")               # Longitude (degrees east)
  lon.edges <- var.get.nc(nc, "lon_edges")   # Longitude edges
  depth <- var.get.nc(nc, "zt")              # Depth (meters)
  depth.edges <- var.get.nc(nc, "zt_edges")  # Depth edges (meters)
  time <- var.get.nc(nc, "time")             # Time (year mid-point)

  # Extract bottom-water oxygen and topography data
  var.arr <- var.get.nc(nc, var)             # Bottom-water oxygen array
  topo <- var.get.nc(nc, "grid_topo")        # Topography (meters)

  # Set the length of time, longitude, and latitude variables
  time <- length(time)
  lon.length <- length(lon)
  lat.length <- length(lat)

  # Initialize arrays to store the bottom-water depth indices and oxygen values for each grid cell
  bw.cells <- array(dim = c(lon.length, lat.length))
  bw.array <- array(dim = c(lon.length, lat.length))

  # Iterate over longitude and latitude grid cells
  for (lon in 1:lon.length){
    for (lat in 1:lat.length){

      # Check for land areas based on topography (NA values)
      if (is.na(topo[lon, lat])){
        # Set land cells to zero and mark oxygen values as NA
        bw.cells[lon, lat] <- 0
        bw.array[lon, lat] <- NA

      } else {
        # Identify the bottom-water cell by finding the closest depth to topography edges
        bw.cells[lon, lat] <- which.min(abs(topo[lon, lat] - depth.edges)) - 1
        # Extract bottom-water oxygen concentration at that depth for the latest time point
        bw.array[lon, lat] <- var.arr[lon, lat, which.min(abs(topo[lon, lat] - depth.edges)) - 1, time]
      }
    }
  }

  # Initialize counters for areas classified as anoxic, suboxic, oxic, and land
  A.anox <- 0
  A.subox <- 0
  A.ox <- 0
  land <- 0

  # Iterate again over longitude and latitude grid cells to classify redox states
  for (lon in 1:lon){
    for (lat in 1:lat){
      if (is.na(topo[lon, lat])) {
        # Increment land area counter
        land <- land + 1
      } else if (bw.array[lon, lat] <= anox.thresh) {
        # Increment anoxic area counter
        A.anox <- A.anox + 1
      } else if (bw.array[lon, lat] > anox.thresh & bw.array[lon, lat] <= subox.thresh) {
        # Increment suboxic area counter
        A.subox <- A.subox + 1
      } else if (bw.array[lon, lat] > subox.thresh) {
        # Increment oxic area counter
        A.ox <- A.ox + 1
      }
    }
  }

  # Calculate the fractions of anoxic, suboxic, and oxic bottom waters
  f.anox <- A.anox / (A.anox + A.subox + A.ox)
  f.subox <- A.subox / (A.anox + A.subox + A.ox)
  f.ox <- A.ox / (A.anox + A.subox + A.ox)

  # Apply a lower limit to the anoxic fraction (0.1%) if `lower.anox.lim` is TRUE
  if (lower.anox.lim == TRUE){
    if (f.anox < 0.001){
      f.anox <- 0.001
    }
  }

  # Create a list to store and return the redox summary (fractions and oxygen array)
  bw.redox.sum <- list(f.anox = f.anox, f.subox = f.subox, f.ox = f.ox, bw.array = bw.array)

  return(bw.redox.sum)
}
