#' Summarize Bottom-Water Environments from cGENIE NetCDF Files
#'
#' This function extracts and summarizes the bottom-water redox state from 3D cGENIE NetCDF files. It retrieves specified variables, computes the corresponding bottom-water depths, and optionally returns the data as either an array or a dataframe.
#'
#' @param var Character string specifying the variable to extract from the NetCDF file.
#' @param experiment Character string indicating the path to the experiment directory containing the NetCDF files.
#' @param dataframe Logical, if TRUE (default), return the data as a dataframe. If FALSE, return the data as an array.
#' @param array Logical, if TRUE, return the extracted bottom-water data as an array. Default is FALSE.
#' @return Depending on the `dataframe` and `array` parameters:
#' \item{dataframe}{A dataframe containing the extracted bottom-water data, including longitude, latitude, and variable values.}
#' \item{array}{A 2D array of the extracted bottom-water variable values if `array` is TRUE.}
#' @examples
#' result_df <- cGENIE.bw("O2", "path/to/experiment", dataframe = TRUE)
#' result_array <- cGENIE.bw("O2", "path/to/experiment", array = TRUE)
#' @import RNetCDF
#' @import dplyr
#' @export
#'
#'
cGENIE.bw <- function(var, experiment, dataframe = TRUE, array = FALSE){

  library(RNetCDF)
  library(dplyr)

  dims <- 3

  nc <- open.nc(paste0(experiment, "/biogem/fields_biogem_", dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees north
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon") # units: degrees east
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt") # units: metres
  depth.edges <- var.get.nc(nc, "zt_edges") # units: metres
  time <- var.get.nc(nc, "time") # units: year mid-point

  # Extract named variables
  var.arr <- var.get.nc(nc, var)
  topo <- var.get.nc(nc, "grid_topo") # units: meters

  # Amend grid to project on 0 degrees
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= - 180] <- lon.edges[lon.edges <= - 180] + 360
    lon[lon <= - 180] <- lon[lon <= - 180] + 360
  }

  lon.length <- length(lon)
  lat.length <- length(lat)
  bw.cells <- array(dim=c(lon.length, lat.length))
  bw.array <- array(dim=c(lon.length, lat.length))

  for(lon.step in 1:lon.length){
    for(lat.step in 1:lat.length){

      if(is.na(topo[lon.step, lat.step])) {
        bw.cells[lon.step, lat.step] <- 0
        bw.array[lon.step, lat.step] <- NA

      } else {
        bw.cells[lon.step, lat.step] <- which.min(abs(topo[lon.step, lat.step] - depth.edges)) - 1
        bw.array[lon.step, lat.step] <- var.arr[lon.step, lat.step, which.min(abs(topo[lon.step, lat.step] - depth.edges)) - 1, length(time)]
      }
    }
  }

  if(array == TRUE){
    return(bw.array)
  }

  if(dataframe == TRUE){
    # Generate dataframe of 2D genie slice from 3D genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(bw.array))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
    return(df)
  }
}
