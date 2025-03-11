#' Summarize the Volume of Oxygen Minimum Zones (OMZs)
#'
#' This function calculates the proportion of the ocean volume that is classified as oxygen minimum zones (OMZs)
#' based on the given threshold of oxygen concentration. The calculation is performed using data from
#' a specified experiment in NetCDF format.
#'
#' @param experiment A string representing the path to the experiment's directory containing the NetCDF files.
#' @param thresh A numeric value specifying the threshold oxygen concentration (in umol/kg) below which
#'                the volume is considered part of the OMZ. Default is 58.54e-6 based on Canfield 60uM
#'                https://enviromicro-journals.onlinelibrary.wiley.com/doi/10.1111/1462-2920.16192
#'                (just a conversion based on global mean density of 1.025 kg/L)
#' @param time.step Either "default" to use the last time step or a numeric index to specify a particular time step.
#'
#' @return A numeric value representing the proportion of the ocean volume that is classified as an OMZ.
#'
#' @details
#' The function extracts relevant variables from the NetCDF file, calculates the thickness of each depth layer,
#' and sums the volumes classified as OMZs and non-OMZs based on the provided threshold. The final output is
#' the proportion of the total volume that constitutes OMZs.
#'
#' @import RNetCDF
#' @import dplyr
#'
#' @export
cGENIE.omz.volume <- function(experiment, thresh = 58.54e-6, time.step = "default") {
  # Load necessary libraries
  library(RNetCDF)

  # Set dimensions
  dims <- 3
  var <- "ocn_O2"

  # Open NetCDF file
  nc <- open.nc(paste0(experiment, "/biogem/fields_biogem_", dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees north
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon") # units: degrees east
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt") # units: metres
  depth.edges <- var.get.nc(nc, "zt_edges") # units: metres
  time <- var.get.nc(nc, "time") # units: year mid-point

  # Determine the time step
  time <- if (time.step == "default") length(time) else time.step

  # Extract variable array
  var.arr <- var.get.nc(nc, var)

  # Calculate depth thicknesses
  depth.thicknesses <- diff(depth.edges)

  # Initialize volume counters
  omz.vol <- 0
  non.omz.vol <- 0
  total.vol <- 0

  # Loop through grid cells to calculate volumes
  for (lon_idx in seq_along(lon)) {
    for (lat_idx in seq_along(lat)) {
      for (depth_idx in seq_along(depth)) {
        oxygen_level <- var.arr[lon_idx, lat_idx, depth_idx, time]

        if (!is.na(oxygen_level)) { # Skip land
          thickness <- depth.thicknesses[depth_idx]
          total.vol <- total.vol + thickness

          if (oxygen_level <= thresh) {
            omz.vol <- omz.vol + thickness
          } else {
            non.omz.vol <- non.omz.vol + thickness
          }
        }
      }
    }
  }

  # Calculate the proportion of OMZ volume
  prop.omz.vol <- omz.vol / total.vol

  return(prop.omz.vol)
}
