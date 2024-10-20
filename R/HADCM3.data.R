#' Extract Data from NetCDF Files
#'
#' This function extracts data from imported NetCDF (.nc) files, specifically designed for use with HADCM3 model outputs.
#'
#' @param var Character. The name of the variable to extract from the NetCDF file.
#' @param file Character. The name of the NetCDF file (without the .nc extension).
#' @param experiment Character. The path to the directory containing the NetCDF file.
#' @param depth.level Numeric. The depth level to extract (default is 1).
#' @param dims Numeric. The number of dimensions of the NetCDF file (default is 3).
#' @param time.present Logical. Whether the time dimension is present in the NetCDF file (default is FALSE).
#'
#' @return A data frame containing the extracted data, with columns for longitude, latitude, and the specified variable.
#'
#' @details
#' This function reads in a NetCDF file using the RNetCDF package and extracts the specified variable. It handles both 2D and 3D data, and can adjust longitude values to be within the range of -180 to 180 degrees. The function also filters the data to ensure it falls within realistic geographic bounds.
#'
#' @note
#' The function assumes that the NetCDF file follows a specific structure, with variables named "latitude", "longitude", "depth_1", and "t" for time. Adjustments may be needed for different file structures.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' df <- HADCM3.data(var = "insitu_T_ym_dpth", file = "o.pgclann", experiment = "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444")
#' }
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#' @import pals
#' @import viridis
#' @export
HADCM3.data <- function(var, file, experiment,
            depth.level = 1,
            dims = 3,
            time.present = FALSE
             ){

  # Load necessary libraries
  # RNetCDF: for reading NetCDF files
  # dplyr: for data manipulation
  # sf: for handling simple features (spatial data)
  # sp: for spatial data classes and methods
  # ggspatial: for spatial data visualization
  # reshape2: for reshaping data
  # ggplot2: for data visualization
  # pals: for color palettes
  # viridis: for color palettes
  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)
  library(pals)
  library(viridis)
  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude and longitude variables
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # Calculate latitude edges for plotting
  lon <- var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # Calculate longitude edges for plotting

  # Extract depth variable if the data is 3D
  if(dims == 3){
    depth <- var.get.nc(nc, "depth_1") # units: metres
    depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # Calculate depth edges
  }

  # Extract time variable if present
  if(time.present == TRUE){
    time <- var.get.nc(nc, "t") # units: year mid-point
  }

  # Extract the specified variable
  var.arr <- var.get.nc(nc, var)

  # Adjust longitude values to be within the range of -180 to 180 degrees
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges > 180] - 360
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  # Create a data frame for 3D data
  if(dims == 3){
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[,, depth.level]))$value))

    # Assign column names to the data frame
    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
  }

  # Create a data frame for 2D data
  if(dims == 2){
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr))$value))

    # Assign column names to the data frame
    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )

    # Special handling for specific file and variable
    if(file == ".qrparm.orog" & var == "ht"){
      df$var <- as.factor(df$var)
      df <- filter(df, var != "0")
      df$var <- as.numeric(paste(df$var))
    }
  }

  # Filter the data frame to ensure realistic geographic bounds
  df <- df %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90
    )

  # Return the final data frame
  return(df)
