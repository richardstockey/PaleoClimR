###################################################
# temp.matching.benthic.coords.R
# Rich Stockey 20240324
# making new benthic version of temp.matching.coords.R
# designed to match palaeocoordinates to  HADCM3 models  by Valdes et al. 2021
###################################################


temp.matching.benthic.coords <- function(var = "insitu_T_ym_dpth", # default assumption is for marine
                                 file = "o.pgclann",
                                 experiment,
                        depth.level = 1,
                        dims = 3,
                        time.present = FALSE,
                        coord.dat, # is any data frame with the lat long column names assigned - HADCM3 data will be added to this and returned
                        lat.name = "p_lat",
                        lng.name = "p_lng"
                        ){


  grid.dat <- HADCM3.grid(file = file,
                          experiment = experiment,
                          dims = dims)


  clim.dat <- NA
  clim.dat <- HADCM3.benthic.data(var = var,
                          file = file,
                          experiment = experiment,
                          depth.level = 1,
                          dims = dims,
                          na.rm = TRUE)

  # # omit NAs in the var value for climate data file
  # clim.dat <- filter(clim.dat, is.na(var) == FALSE)

  # remove any NA paleocoordinates
  # at somepoint check how many records this impacts
  coord.dat <- filter(coord.dat, is.na(p_lng) == FALSE &  is.na(p_lat) == FALSE )

  # sure there is an elegant way to do this without a loop. return to when there is time.
  for(row in 1:nrow(coord.dat)){

    # find mid-point of 'nearest' latitudinal grid cell for each occurrence
    coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat$p_lat[row]-grid.dat$lat))]

    # identify all of the cells int the whole climate model that have the same latitude as the fossil.
    # (need to do one first, but reconstructed latitude is expected to be more accurate than longitude for palaeomag reasons)
    lat.mid.opts <- clim.dat %>%
      filter(lat.mid == coord.dat$lat.bin.mid[row])

    # if there are latitudinal mid opts (why wouldnt there be, can we get rid of this?) and the difference between
    # the closest longitudinal bin to the fossils and the closest (in longitude) climate cell is less than 30 degrees, then
    # assign the closest longitudinal cell.
    # maybe reduce the 30 degree figure. 10?
    # 20240115 - changing to 10 and planning to reflect more on it.
    if(nrow(lat.mid.opts > 0) & min(abs(coord.dat$lng[row]-lat.mid.opts$lon.mid)) < 10){

      coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat$lng[row]-lat.mid.opts$lon.mid))]

      # then assign HADCM3.var based on the assigned latitudinal and longitudinal bins!
      coord.dat$HADCM3.var[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row] ]

      #print(paste0("row number ", row))
    }else{ # if there is only land available at that latitude or the nearest grid cell is more than XX degrees away (could change?), just put an NA in climate var file.

      coord.dat$lon.bin.mid[row] <- NA
      coord.dat$HADCM3.var[row] <- NA

    }
  }

  coord.dat <- filter(coord.dat, is.na(HADCM3.var) == FALSE)

  return(coord.dat)

}

