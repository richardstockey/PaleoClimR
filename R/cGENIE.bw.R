###################################################
# cGENIE.bw.R
# Rich Stockey 20230922
# designed to summarise the redox of bottom-water environments from cGENIE .nc files (imported using cGENIE.nc.import.R)
###################################################
# full comments to follow...

cGENIE.bw <- function(var, experiment, dataframe = TRUE, array = FALSE){
  # -------------------------------------------------------------------------------------------------------
  # anox.thresh = 0 umol/kg (i.e. no oxygen) is set as default.
  # subox.thresh = 4.8 umol/kg from Sperling et al. 2015) is set as default.
  # -------------------------------------------------------------------------------------------------------
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


  # amend grid to project on 0 degs - note cGENIE differs from HADCM3
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= - 180] <- lon.edges[lon.edges <= - 180] + 360
    lon[lon <= - 180] <- lon[lon <= - 180] + 360
  }

  time <- length(time)
  lon.length <- length(lon)
  lat.length <- length(lat)
  bw.cells <- array(dim=c(lon.length,lat.length))
  bw.array <- array(dim=c(lon.length,lat.length))

  for(lon.step in 1:lon.length){
    for(lat.step in 1:lat.length){

      if(is.na(topo[lon.step,lat.step]) == TRUE){

        bw.cells[lon.step, lat.step] <- 0
        bw.array[lon.step, lat.step] <- NA

      }else{
        bw.cells[lon.step, lat.step] <- which.min(abs(topo[lon.step,lat.step]-depth.edges))-1
        bw.array[lon.step, lat.step] <- var.arr[lon.step, lat.step, which.min(abs(topo[lon.step,lat.step]-depth.edges))-1, time]
      }
    }
  }

if(array == TRUE){
  return(bw.array)
}

  if(dataframe == TRUE){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
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
