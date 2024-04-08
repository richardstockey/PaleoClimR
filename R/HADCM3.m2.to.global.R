###################################################
# HADCM3.m2.to.global.R
# Rich Stockey 20240408
# designed to upscale per area estimates of things like triffid outputs to global values...
###################################################
# full comments to follow...

HADCM3.m2.to.global <- function(var, file, experiment,
                        depth.level = 1,
                        dims = 2,
                       unit.factor = 1,
                       time.present = FALSE){

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
  library(geosphere)

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
  if(dims == 3){
  depth <- var.get.nc(nc, "depth_1") # units: metres
  depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep
  }
  if(time.present == TRUE){
  time <- var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
  # note that not all of these general variables will be available for fields_biogem_2d (address later)
}
  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  # NOTE - this is what i have done with cGENIE models.
  # Is this the best way to deal with here,
  # or just another way of translating to a nice grid?
  # maybe for plotting either is kind of fine.
  # but definitely would need to be fixed for point data matching.
  # deal with weird lon coordinates if present
  # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # amend HADCM3 grid to project on 0 degs
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges >180] - 360
    lon[lon >180] <- lon[lon >180] -360
  }


  if(dims == 3){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      #as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))
      as.data.frame(melt(var.arr[,, depth.level]))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
  }
  if(dims == 2){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      #as.data.frame(melt(var.arr[,, time.step]))$value))
      as.data.frame(melt(var.arr))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
  }

  # eliminate cells outside of reasonable range
  df <- df %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90,
           lat.max >= -90, # ADDING THIS ON 20240408 AS FOR TRIFFIC FILES COUNTING SEEMS TO BE FLIPPED...
           lat.min <= 90
    )

  # NOTE - NOT SURE I HAVE A PERFECT UNDERSTANDING OF THE HADCM3 grid geometry but this should be pretty much correct as looks fine in map view
  # Sit down with BRIDGE group at somepoint?

  # SCRAPPED THIS FOR TOTAL CALCULATIONS...
  # # also eliminate cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  # df$lon.range <- abs(df$lon.min-df$lon.max)
  # df <- df %>%
  #   filter(lon.range < 180 #could just be greater than 4, but this will work for all model grids
  #   )



  poly.list <- list()
  poly.names.list <- list()
  for(poly in 1:(nrow(df))){

    polygon.code <- Polygon(cbind(
      c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
      c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
    assign(paste0("Polygon_", poly), polygon.code)

    polygons.code <- Polygons(list(polygon.code), paste0("p",poly))
    assign(paste0("Polygons_", poly), polygons.code)

    poly.list <- append(poly.list, polygons.code)
    poly.names.list <- append(poly.names.list, paste0("p",poly))
  }

  SpP <- SpatialPolygons(poly.list)

  area_m2 <- areaPolygon(SpP)

  df <- cbind(df, area_m2)


  df$TotalCellVal <- df$var*df$area_m2

  # NOTE - an implicit check of this function is to check the area of the Earth calculated
  # For Sarkar 2022 experiments, sum(df$area_m2) gives 5.099432e+14, which is approximately the same as the 'real' [spherical] value of 5.100644719×10¹⁴ m²https://www.quora.com/What-is-the-exact-surface-area-of-earth-in-square-meters#:~:text=The%20earth's%20radius%20is%206.371,%C2%B2%20%3D%205.100644719%C3%9710%C2%B9%E2%81%B4%20m%C2%B2.

  TotalVal <- sum(df$TotalCellVal, na.rm = TRUE)
  return(TotalVal)
}


