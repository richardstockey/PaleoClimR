###################################################
# cGENIE.bw.redox.R
# Rich Stockey 20230922
# designed to summarise the redox of bottom-water environments
# this is done from raw experiments rather than using cGENIE.nc.import.R because we also need topo grids
# we use borrow code from cGENIE.bw.R (don't just use function as we need more variables) and then synthesise redox bins
###################################################
# full comments to follow...

cGENIE.bw.redox <- function(experiment, anox.thresh = 0, subox.thresh = 4.8e-6){
  # -------------------------------------------------------------------------------------------------------
  # anox.thresh = 0 umol/kg (i.e. no oxygen) is set as default.
  # subox.thresh = 4.8 umol/kg from Sperling et al. 2015) is set as default.
  # -------------------------------------------------------------------------------------------------------
  library(RNetCDF)
  library(dplyr)
  dims <- 3
  var <- "ocn_O2"
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

  time <- length(time)
  lon.length <- length(lon)
  lat.length <- length(lat)
  bw.cells <- array(dim=c(lon.length,lat.length))
  bw.array <- array(dim=c(lon.length,lat.length))

  for(lon in 1:lon.length){
    for(lat in 1:lat.length){

      if(is.na(topo[lon,lat]) == TRUE){

        bw.cells[lon, lat] <- 0
        bw.array[lon, lat] <- NA

      }else{
        bw.cells[lon, lat] <- which.min(abs(topo[lon,lat]-depth.edges))-1
        bw.array[lon, lat] <- var.arr[lon, lat, which.min(abs(topo[lon,lat]-depth.edges))-1, time]
      }
    }
  }

    A.anox <- 0
    A.subox <- 0
    A.ox <- 0
    land <- 0
    for(lon in 1:lon){
      for(lat in 1:lat){
        if(is.na(topo[lon,lat])==TRUE){
          land <- land +1
        }else if(bw.array[lon, lat] <= anox.thresh){
          A.anox <- A.anox + 1
        }else if(bw.array[lon, lat] > anox.thresh & bw.array[lon, lat] <= subox.thresh){
          A.subox <- A.subox + 1
        }else if(bw.array[lon, lat] > subox.thresh){
          A.ox <- A.ox + 1
        }
      }
    }


    f.anox <- A.anox/(A.anox + A.subox + A.ox)
    f.subox <- A.subox/(A.anox + A.subox + A.ox)
    f.ox <- A.ox/(A.anox + A.subox + A.ox)

    if(f.anox < 0.001){
      f.anox <- 0.001
    }

    bw.redox.sum <- list(f.anox = f.anox, f.subox = f.subox,  f.ox = f.ox, bw.array = bw.array)

  return(bw.redox.sum)
}
