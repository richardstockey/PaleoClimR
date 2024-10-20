#' cGENIE Point Matching
#'
#' This function matches palaeocoordinates to cGENIE model data.
#'
#' @param var Character. The variable to extract from the cGENIE model.
#' @param experiment Character. The experiment identifier for the cGENIE model.
#' @param depth.level Numeric. The depth level to extract from the cGENIE model. Default is 1.
#' @param dims Numeric. The number of dimensions in the cGENIE model. Default is 3.
#' @param time.present Logical. Whether to use the present time for the cGENIE model. Default is FALSE.
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#'
#' @return A data frame with the original coordinates and matched climate data from the cGENIE model.
#'
#' @examples
#' \dontrun{
#' coord.dat <- data.frame(p_lat = c(-30, 45), p_lng = c(120, -75))
#' result <- cGENIE.point.matching(var = "temperature", experiment = "experiment1", coord.dat = coord.dat)
#' }
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#' @export

cGENIE.point.matching <- function(var = NULL, 
                                  experiment = NULL,
                                  depth.level = 1,
                                  dims = 3,
                                  coord.dat = NULL, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                                  lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                                  lng.name = "p_lng") # name IF generated from rotated paleoverse coordinates...
{
  
# Load necessary libraries
library(RNetCDF)  # For handling NetCDF files
library(dplyr)    # For data manipulation
library(reshape2) # For reshaping data
# Extract grid data from cGENIE netCDF file
# This function call retrieves the grid data for the specified experiment and dimensions.
# The grid data contains information about the spatial layout of the cGENIE model.
grid.dat <- cGENIE.grid(experiment = experiment, dims = dims)

# Extract climate data from cGENIE netCDF file
# This function call retrieves the climate data for the specified variable, experiment, depth level, and dimensions.
# The 'year' parameter is set to "default" to use the default time slice.
clim.dat <- cGENIE.data(var = var, experiment = experiment, depth.level = depth.level, dims = dims, year = "default")

# Omit NAs in the var value for climate data file
# This step filters out any rows in the climate data where the specified variable has NA values.
# This ensures that only valid data points are used in the matching process.
clim.dat <- filter(clim.dat, !is.na(var))
# Remove any NA paleocoordinates
# This step filters out any rows in the coordinate data where the latitude or longitude values are NA.
# This ensures that only valid coordinates are used in the matching process.
coord.dat <- filter(coord.dat, !is.na(!!sym(lng.name)) & !is.na(!!sym(lat.name)))

# Initialize a column for matched climate data
# This step adds a new column to the coordinate data frame to store the matched climate data.
# Initially, all values in this column are set to NA.
coord.dat$matched_climate <- NA
  
# Iterate over each row in the coordinate data frame
# This loop processes each coordinate point to find the matching climate data.
for(row in 1:nrow(coord.dat)){
    
    # Find the mid-point of the nearest latitudinal grid cell for each occurrence
    # This step identifies the closest latitude grid point in the cGENIE model to the current coordinate's latitude.
    coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat[[lat.name]][row] - grid.dat$lat))]
    
    # Identify all the cells in the climate model that have the same latitude as the data point
    # This step filters the climate data to include only the rows where the latitude matches the closest latitude grid point.
    lat.mid.opts <- clim.dat %>%
        filter(lat.mid == coord.dat$lat.bin.mid[row])
    
    # Check if there are any matching latitude options and if the closest longitudinal bin is within 10 degrees
    # This condition ensures that there are valid latitude matches and that the closest longitude grid point is within a reasonable distance.
    if(nrow(lat.mid.opts) > 0 & min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid)) < 10){
        
        # Find the mid-point of the nearest longitudinal grid cell
        # This step identifies the closest longitude grid point in the cGENIE model to the current coordinate's longitude.
        coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid))]
        
        # Assign the matched climate data based on the assigned latitudinal and longitudinal bins
        # This step retrieves the climate data value for the closest latitude and longitude grid points and assigns it to the matched_climate column.
        coord.dat$matched_climate[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row]]
        
    } else {
        # If there are no valid latitude matches or the nearest longitude grid cell is more than 10 degrees away, assign NA
        # This step handles cases where the coordinate point is too far from any valid climate data points.
        coord.dat$lon.bin.mid[row] <- NA
        coord.dat$matched_climate[row] <- NA
    }
}

# Filter out rows where the matched climate data is NA
# This step removes any coordinate points that did not have valid climate data matches.
coord.dat <- filter(coord.dat, is.na(matched_climate) == FALSE)
  
names(coord.dat)[1:2] <- c("lat", "lng")
    
return(coord.dat)
  
}
