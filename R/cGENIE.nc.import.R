###################################################
# cGENIE.nc.import.R
# Rich Stockey 20230922
# designed to import .nc files
###################################################
# full comments to follow...

cGENIE.nc.import <- function(var, experiment, dims = 3, model = "biogem"){
  library(RNetCDF)
  library(dplyr)

  if(model == "biogem"){
    prefix <- "/biogem/fields_biogem_"
  }

  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees north
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon") # units: degrees east
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt") # units: metres
  depth.edges <- var.get.nc(nc, "zt_edges") # units: metres
  time <- var.get.nc(nc, "time") # units: year mid-point
  # note that not all of these general variables will be available for fields_biogem_2d (address later)

  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  nc.sum <- list(lat = lat, lon = lon, depth = depth, time = time, var = var.arr)

  return(nc.sum)

}
