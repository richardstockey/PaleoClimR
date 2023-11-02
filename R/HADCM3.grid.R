###################################################
# HADCM3.grid.R
# Rich Stockey 20231102
# designed to extract grid data from imported .nc files (from e.g. from Valdes et al. 2021)
###################################################
# full comments to follow...

HADCM3.grid <- function(var, file, experiment){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default
  # palette_name currently has to be followed by (1000) or some other number
  # other options than parula would include - viridis and the many other options here https://r-charts.com/color-palettes/
  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)
  library(pals)
  library(viridis)

  library(dplyr)
  #experiment <- "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444"
  #file <- "o.pgclann"
  #var <- "insitu_T_ym_dpth"
  # can set things up so that "if var == xxx, then file <- yyy"

  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract general variables
  # NOTE - these may not precisely represent the HADCM3 grid
  # fudged slightly for ease of plotting from the xxx and xxx_1 variables.
  # worth checking with HADCM3 users to be doubly sure
  # this should be kept separate from variable matching scripts with point data such as PBDB and therefore be functionally fine
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  depth <- var.get.nc(nc, "depth_1") # units: metres
  depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep

  # amend HADCM3 grid to project on 0 degs
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] -360
    lon[lon >180] <- lon[lon >180] -360
  }


lat <- lat[lat<90 & lat>-90]
lat.edges <- lat.edges[lat.edges<90 & lat.edges>-90]
lon <- lon[lon<180 & lon>-180]
lon.edges <- lon.edges[lon.edges<180 & lon.edges>-180]


grid <- list(
  lat,
  lat.edges,
  lon.edges,
  depth,
  depth.edges
)
names(grid) <- c(
  "lat",
  "lat.edges",
  "lon.edges",
  "depth",
  "depth.edges"
)

return(grid)
}


