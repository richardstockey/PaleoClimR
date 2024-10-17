###################################################
# cGENIE.data.R
# Rich Stockey 20231105
# Function designed to extract 2D lat-lon data fields from imported 2D or 3D .nc files
###################################################

# cGENIE.data
# Extracts 2D data fields from 3D or 2D netCDF (.nc) files.
#
# Arguments:
# var            - The variable name to extract from the .nc file.
# experiment     - Directory containing the experiment's netCDF files.
# depth.level    - Depth level to extract the data from. Set to NULL by default.
# dims           - Number of dimensions in the netCDF file. Defaults to 3.
# year           - Year to extract data for (default is "default", meaning the last time point).
# model          - The model type; defaults to "biogem".
#
# Returns:
# A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
###################################################

cGENIE.data <- function(var, experiment,
                        depth.level = NULL,
                        dims = 3,            # Set default dims to 3
                        year = "default",
                        model = "biogem"){

  library(RNetCDF)
  library(dplyr)
  library(reshape2)

  # Define the prefix based on the selected model
  if (model == "biogem") {
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the netCDF file based on the dimensions
  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat")            # Latitude (degrees north)
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon")            # Longitude (degrees east)
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt")           # Depth (meters)
  depth.edges <- var.get.nc(nc, "zt_edges")
  time <- var.get.nc(nc, "time")          # Time (year mid-point)

  # Extract the target variable
  var.arr <- var.get.nc(nc, var)

  # Set time step based on the year argument
  if (year == "default") {
    time.step <- length(time)  # Last time point
  } else {
    time.step <- year
  }

  # Set depth.level to 1 if dims == 3 and depth.level is NULL
  if (dims == 3 && is.null(depth.level)) {
    depth.level <- 1
  }

  # Handle longitude adjustments (to avoid values outside -180 to 180 range)
  if (mean(between(lon, -180, 180)) < 1) {
    lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
    lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Extract data based on dimensionality (dims)
  if (dims == 3) {
    # Create data frame for 2D slice from 3D array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))

    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  if (dims == 2) {
    # Create data frame for 2D array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[,, time.step]))$value))

    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  # Filter the data to ensure valid lat-lon ranges
  df <- df %>%
    filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

  # Handle cells at the extremes (-180 and 180 longitude)
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

  return(df)
}
