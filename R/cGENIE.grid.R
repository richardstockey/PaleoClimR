###################################################
# cGENIE.grid.R
# Rich Stockey 20231105
# designed to extract grid data from imported cGENIE .nc files
###################################################
# full comments to follow...

cGENIE.grid <- function(experiment, dims, model = "biogem"){

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

  # amend grid to project on 0 degs - note cGENIE differs from HADCM3
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= - 180] <- lon.edges[lon.edges <= - 180] + 360
    lon[lon <= - 180] <- lon[lon <= - 180] + 360
  }


lat <- lat[lat<90 & lat>-90]
lat.edges <- lat.edges[lat.edges<90 & lat.edges>-90]
lon <- lon[lon<180 & lon>-180]
lon.edges <- lon.edges[lon.edges<180 & lon.edges>-180]

if(dims == 3){
grid <- list(
  lat,
  lat.edges,
  lon,
  lon.edges,
  depth,
  depth.edges
)
names(grid) <- c(
  "lat",
  "lat.edges",
  "lon",
  "lon.edges",
  "depth",
  "depth.edges"
)
}
if(dims == 2){
  grid <- list(
    lat,
    lon,
    lat.edges,
    lon.edges
  )
  names(grid) <- c(
    "lat",
    "lat.edges",
    "lon",
    "lon.edges"
  )
}
return(grid)
}


