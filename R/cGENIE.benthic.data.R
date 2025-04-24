uy#' Summarize Bottom-Water Environments from cGENIE netCDF Files
#'
#' This function extracts and summarizes the bottom-water redox state from 3D cGENIE netCDF files. It retrieves specified variables, computes the corresponding bottom-water depths, and optionally returns the data as either an array or a dataframe.
#'
#' @param var \code{character}. Variable name to extract from cGENIE netCDF file.
#' @param experiment \code{character}. A character string specifying the path to the experiment folder where the target cGENIE netCDF files are stored.
#' @param output \code{character}. The format of the output. Default is "dataframe". Options are "dataframe" or "array".
#' @return A data frame or 3D array with the original coordinates and matched climate data from the cGENIE model.
#' @import RNetCDF
#' @import reshape2
#' @import dplyr
#' @export
#'
#'
#'
cGENIE.benthic.data <- function(var, experiment, output = "dataframe"){

  # Load required libraries
  library(RNetCDF)  # For reading netCDF files

  # Define the dimension (3D grid) of the data
  dims <- 3

  # Open the netCDF file located in the experiment directory
  nc <- RNetCDF::open.nc(paste0(experiment, "/biogem/fields_biogem_", dims, "d", ".nc"))

  # Extract general grid variables: latitude, longitude, depth, and time
  lat <- RNetCDF::var.get.nc(nc, "lat")              # Latitude in degrees north
  lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")  # Latitude edges
  lon <- RNetCDF::var.get.nc(nc, "lon")              # Longitude in degrees east
  lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")  # Longitude edges
  depth <- RNetCDF::var.get.nc(nc, "zt")             # Depth in meters
  depth.edges <- RNetCDF::var.get.nc(nc, "zt_edges") # Depth edges in meters
  time <- RNetCDF::var.get.nc(nc, "time")            # Time (year mid-point)

  # Extract the specified variable (e.g., oxygen, temperature) from the netCDF file
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # Extract topography data to determine ocean floor depth
  topo <- RNetCDF::var.get.nc(nc, "grid_topo")       # Topography in meters

  # Adjust longitude coordinates to ensure they are projected between 0 and 360 degrees
  if(mean(dplyr::between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
    lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Determine the lengths of longitude and latitude grids
  lon.length <- length(lon)
  lat.length <- length(lat)

  # Initialize arrays to store bottom-water cell indices and values
  bw.cells <- array(dim = c(lon.length, lat.length))
  bw.array <- array(dim = c(lon.length, lat.length))

  # Loop through all longitude and latitude points to map bottom-water values
  for(lon.step in 1:lon.length){
    for(lat.step in 1:lat.length){

      # If the cell is over land (i.e., topography is NA), set the cell to land
      if(is.na(topo[lon.step, lat.step])) {
        bw.cells[lon.step, lat.step] <- 0
        bw.array[lon.step, lat.step] <- NA   # Mark the value as NA for land cells

      } else {
        # For ocean points, find the bottom-most ocean layer by comparing topography with depth edges
        bw.cells[lon.step, lat.step] <- which.min(abs(topo[lon.step, lat.step] - depth.edges)) - 1
        # Extract the bottom-water value of the specified variable at the closest depth for the latest time point
        bw.array[lon.step, lat.step] <- var.arr[lon.step, lat.step, which.min(abs(topo[lon.step, lat.step] - depth.edges)) - 1, length(time)]
      }
    }
  }

  # Return the array of bottom-water values if output == "array"
  if(output == "array"){
    return(bw.array)
  }

  # If output == "dataframe", convert the data into a structured dataframe
  if(output == "dataframe"){
    # Create a dataframe by binding longitude, latitude, and the respective edges with the extracted variable values
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),                            # Longitude mid-points
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1), # Longitude min (edges)
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),     # Longitude max (edges)
      rep(lat, times = 1, each = length(lon)),                            # Latitude mid-points
      rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)), # Latitude min (edges)
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),     # Latitude max (edges)
      as.data.frame(reshape2::melt(bw.array))$value))                               # Extracted variable values as 2D matrix

    # Rename the dataframe columns for clarity
    names(df) <- c("lon.mid",   # Longitude mid-point
                   "lon.min",   # Minimum longitude (from edges)
                   "lon.max",   # Maximum longitude (from edges)
                   "lat.mid",   # Latitude mid-point
                   "lat.min",   # Minimum latitude (from edges)
                   "lat.max",   # Maximum latitude (from edges)
                   "var"        # Extracted variable value
    )

    return(df)  # Return the final dataframe
  }
}
