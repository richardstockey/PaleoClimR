###################################################
# scotese.matrix.R
# Rich Stockey 20231124
# designed to make matrices from imported .nc files (from https://zenodo.org/records/5460860)
###################################################
# full comments to follow...

scotese.matrix <- function(map, # netcdf file
                       min.value = -6000,
                       max.value = 6000,
                       intervals = 2000,
                       continents.outlined,
                       scale.label,
                       res = "deg",
                       scale = "viridis",
                       projection = 'ESRI:54012'){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default

  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)

  nc <- open.nc(paste0(map))

  if(res == "deg"){
    # Extract general variables
    lat <- var.get.nc(nc, "lat") # units: degrees
    lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
    lon <- var.get.nc(nc, "lon") # units: degrees
    lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

  }
  if(res == "6min"){
  # Extract general variables
  lat <- var.get.nc(nc, "latitude") # units: degrees
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "longitude") # units: degrees
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  }
z <- var.get.nc(nc, "z") # units: metres
return(z)
}


