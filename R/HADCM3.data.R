###################################################
# HADCM3.data.R
# Rich Stockey 20231102
# designed to extract data from imported .nc files (from e.g. from Valdes et al. 2021)
###################################################
# full comments to follow...

HADCM3.data <- function(var, file, experiment,
                        depth.level = 1,
                        dims = 3,
                       time.present = FALSE
                       ){

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

  # amend HADCM3 grid to project on 0 degs
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] - 360
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

    if(file == ".qrparm.orog" & var == "ht"){
    df$var <-as.factor(df$var)
    df <- filter(df, var != "0")
    df$var <-as.numeric(paste(df$var))
    }
  }

    df <- df %>%
      filter(lon.max <= 180,
             lon.min >= -180,
             lat.max <= 90,
             lat.min >= -90
             )
return(df)
}


