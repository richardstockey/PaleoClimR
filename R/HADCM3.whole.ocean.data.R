#' Extract Data from HADCM3 NetCDF Files
#'
#' This function extracts data from imported NetCDF (.nc) files, specifically designed for the HADCM3 model.
#' It processes the data to generate a dataframe suitable for further analysis and visualization.
#'
#' @param var Character. The name of the variable to extract from the NetCDF file.
#' @param file Character. The name of the NetCDF file (without extension) to read from.
#' @param experiment Character. The path to the experiment directory containing the NetCDF file.
#' @param time.present Logical. If TRUE, extracts the time variable from the NetCDF file. Default is FALSE.
#' @param na.rm Logical. If TRUE, removes rows with NA values from the resulting dataframe. Default is FALSE.
#'
#' @return A dataframe with the following columns:
#' \describe{
#'   \item{lon.mid}{Numeric. The mid-point longitude values.}
#'   \item{lon.min}{Numeric. The minimum longitude values.}
#'   \item{lon.max}{Numeric. The maximum longitude values.}
#'   \item{lat.mid}{Numeric. The mid-point latitude values.}
#'   \item{lat.min}{Numeric. The minimum latitude values.}
#'   \item{lat.max}{Numeric. The maximum latitude values.}
#'   \item{depth.level}{Numeric. The depth level index.}
#'   \item{var}{Numeric. The extracted variable values.}
#' }
#'
#' @details
#' This function reads a 3D array from the specified NetCDF file and processes it into a 2D dataframe.
#' It adjusts the longitude values to be within the range of -180 to 180 degrees if necessary.
#' The function also handles the extraction of latitude, longitude, and depth variables, and can optionally
#' extract the time variable if `time.present` is set to TRUE.
#'
#' @note
#' The function assumes that the NetCDF file follows the HADCM3 grid conventions. It may require adjustments
#' for other grid configurations.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' df <- HADCM3.whole.ocean.data(var = "insitu_T_ym_dpth", file = "o.pgclann", experiment = "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444")
#' }
#'
#' @export
HADCM3.whole.ocean.data <- function(var, file, experiment,
             time.present = FALSE,
             na.rm = FALSE
             ){

  # Load necessary libraries
  library(RNetCDF)
  library(dplyr)
  library(reshape2)

  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude values from the NetCDF file
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  # Calculate latitude edges for plotting purposes
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2))

  # Extract longitude values from the NetCDF file
  lon <- var.get.nc(nc, "longitude") # units: degrees east
  # Calculate longitude edges for plotting purposes
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2))

  # Extract depth values from the NetCDF file
  depth <- var.get.nc(nc, "depth_1") # units: metres
  # Calculate depth edges for plotting purposes
  depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5))

  # Extract time values if time.present is TRUE
  if(time.present == TRUE){
  time <- var.get.nc(nc, "t") # units: year mid-point
  }

  # Extract named variable from the NetCDF file
  var.arr <- var.get.nc(nc, var)

  # Adjust HADCM3 grid to project on 0 degrees if necessary
  if(mean(between(lon, -180, 180)) < 1){
    # Adjust longitude edges greater than 180 degrees to be within -180 to 180 range
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges > 180] - 360
    # Adjust longitude values greater than 180 degrees to be within -180 to 180 range
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  # Initialize an empty dataframe to store the results
  df.sum <- c()

  # Loop through each depth level
  for(depth.level in 1:length(depth)){
    # Generate a dataframe of 2D slices from the 3D array for the current depth level
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1), # Repeat longitude values for each latitude
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1), # Repeat longitude edge min values for each latitude
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1), # Repeat longitude edge max values for each latitude
      rep(lat, times = 1, each = length(lon)), # Repeat latitude values for each longitude
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)), # Repeat latitude edge min values for each longitude
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)), # Repeat latitude edge max values for each longitude
      rep(depth.level, times = length(lat) * length(lon)), # Repeat depth level index for each combination of latitude and longitude
      as.data.frame(melt(var.arr[,, depth.level]))$value # Extract variable values for the current depth level
    ))

    # Append the dataframe for the current depth level to the results dataframe
    df.sum <- rbind(df.sum, df)
  }

  # Assign column names to the results dataframe
  names(df.sum) <- c("lon.mid",
                     "lon.min",
                     "lon.max",
                     "lat.mid",
                     "lat.min",
                     "lat.max",
                     "depth.level",
                     "var"
  )

  # Filter the results dataframe to ensure longitude and latitude values are within valid ranges
  df.sum <- df.sum %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90
    )

  # Remove rows with NA values if na.rm is TRUE
  if(na.rm == TRUE){
    df.sum <- na.omit(df.sum)
  }

  # Return the final results dataframe
  return(df.sum)
}
