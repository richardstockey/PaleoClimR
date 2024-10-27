#' HADCM3 Point Matching Benthic
#'
#' This function matches palaeocoordinates to cGENIE models by extracting grid and climate data from cGENIE netCDF files and matching them to the provided coordinates.
#' This version of the function is specifically designed for benthic data.
#'
#' @param var Character. The variable to extract from the climate data file. Default is NULL.
#' @param file Character. The file name of the cGENIE netCDF file. Default is NULL.
#' @param experiment Character. The experiment name in the cGENIE netCDF file. Default is NULL.
#' @param dims Numeric. The number of dimensions in the climate data file. Default is 3.
#' @param time.present Logical. Whether the time is present in the climate data file. Default is FALSE.
#' @param coord.dat Data frame. A data frame with the latitude and longitude column names assigned. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in the coord.dat data frame. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in the coord.dat data frame. Default is "p_lng".
#'
#' @return A data frame with the matched climate data added.
#'
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#' @export
HADCM3.point.matching.benthic <- function(var = NULL,
                                  file = NULL,
                                  experiment = NULL,
                                  dims = 3,
                                  time.present = FALSE,
                                  coord.dat, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                                  lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                                  lng.name = "p_lng") # name IF generated from rotated paleoverse coordinates...
{

  # Load necessary libraries
  library(RNetCDF)
  library(dplyr)
  library(reshape2)

  # Extract grid data from cGENIE netCDF file
  grid.dat <- HADCM3.grid(file = file,
                          experiment = experiment,
                          dims = dims)

  # Extract climate data from cGENIE netCDF file
  clim.dat <- HADCM3.benthic.data(var = var,
                          file = file,
                          experiment = experiment,
                          dims = dims)

  # Omit NAs in the var value for climate data file
  clim.dat <- filter(clim.dat, !is.na(var))

  # Remove any NA paleocoordinates
  coord.dat <- filter(coord.dat, !is.na(!!sym(lng.name)) & !is.na(!!sym(lat.name)))

  # Initialize a column for matched climate data
  coord.dat$matched_climate <- NA

  # sure there is an elegant way to do this without a loop. return to when there is time.
  for(row in 1:nrow(coord.dat)){

    # find mid-point of 'nearest' latitudinal grid cell for each occurrence
    coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat$p_lat[row]-grid.dat$lat))]

    # identify all of the cells int the whole climate model that have the same latitude as the data point.
    # (need to do one first, but reconstructed latitude is expected to be more accurate than longitude for palaeomag reasons)
    lat.mid.opts <- clim.dat %>%
      filter(lat.mid == coord.dat$lat.bin.mid[row])

    # if there are latitudinal mid opts (why wouldnt there be, can we get rid of this?) and the difference between
    # the closest longitudinal bin to the fossils and the closest (in longitude) climate cell is less than 10 degrees, then
    # assign the closest longitudinal cell.
    if(nrow(lat.mid.opts > 0) & min(abs(coord.dat$p_lng[row]-lat.mid.opts$lon.mid)) < 10){

      coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat$p_lng[row]-lat.mid.opts$lon.mid))]
      # then assign matched_climate based on the assigned latitudinal and longitudinal bins!
      coord.dat$matched_climate[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row] ]

      #print(paste0("row number ", row))
    }else{ # if there is only land available at that latitude or the nearest grid cell is more than XX degrees away (could change?), just put an NA in climate var file.

      coord.dat$lon.bin.mid[row] <- NA
      coord.dat$matched_climate[row] <- NA

    }
  }

  coord.dat <- filter(coord.dat, is.na(matched_climate) == FALSE)

  names(coord.dat)[1:2] <- c("lat", "lng")

  return(coord.dat)

}
