###################################################
# cGENIE.omz.volume.R
# Rich Stockey 20230922
# designed to summarise the volume of OMZs
# this is done from raw experiments
###################################################
# full comments to follow...

cGENIE.omz.volume <- function(experiment, thresh = 4.8e-6, time.step = "default"){
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

  if(time.step == "default"){
  time <- length(time)
  }
  var.arr <- var.get.nc(nc, var)

  depth.thicknesses <- depth.edges[2:(length(depth.edges))] - depth.edges[1:(length(depth.edges)-1)]

  omz.vol <- 0
  non.omz.vol <- 0
  total.vol <- 0

  for(lon in 1:length(lon)){
    for(lat in 1:length(lat)){
      for(depth in length(depth)){


        if(is.na(var.arr[lon, lat, depth, time]) == TRUE){ #if we're on land
          # do nothing!
          #print("we're on land")
        }else{
          if(var.arr[lon, lat, depth, time] <= thresh){
            omz.vol <- omz.vol + (1 * depth.thicknesses[depth])

          }
          f(var.arr[lon, lat, depth, time] > thresh){
            non.omz.vol <- non.omz.vol + (1 * depth.thicknesses[depth])

          }
        total.vol <- total.vol + (1 * depth.thicknesses[depth])

        }

      }}}

  print(paste("omz vol is", omz.vol, "non omz vol is", non.omz.vol, "total (rel) ocean vol is", total.vol))
  prop.omz.vol <- omz.vol/total.vol

  return(prop.omz.vol)
}
