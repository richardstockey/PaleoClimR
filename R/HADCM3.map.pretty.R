###################################################
# HADCM3.map.pretty.R
# Rich Stockey 20231101
# designed to make pretty maps from imported .nc files (from e.g. from Valdes et al. 2021)
# aimed to include key information to visualise as much of earth system as possible at one time
###################################################
# full comments to follow...

HADCM3.map.pretty <- function(experiment,
                        depth.level = 1,
                        dims = 3,
                       land.opt = "default",
                       min.value,
                       max.value,
                       intervals,
                       continents.outlined,
                       scale.label,
                       unit.factor = 1,
                       time.present = FALSE,
                       scale = "viridis",
                       projection = 'ESRI:54012',
                       calcs = TRUE,
                       plot = TRUE,
                       polygons){

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
  library(ggnewscale)

  palette_name_ocean <- pals::parula(1000)
  #palette_name_ocean <- viridis::magma(1000)
  #palette_name_ocean <- viridis::plasma(1000)
  #palette_name_land <- paletteer::paletteer_c("grDevices::Terrain 2", 30)
  palette_name_land <- paletteer::paletteer_c("grDevices::Light Grays", 30)



  #experiment <- "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444"
  file <- "o.pgclann"
  var <- "insitu_T_ym_dpth"
  # can set things up so that "if var == xxx, then file <- yyy"
  if(calcs == TRUE){
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
  if(time.present == TRUE){
  time <- var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
  # note that not all of these general variables will be available for fields_biogem_2d (address later)
  }

  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  # now we're going to load a second netcdf with orography values

  file_2 <- ".qrparm.orog"
  var_2 <- "ht"

  nc_2 <- open.nc(paste0(experiment, file_2, ".nc"))

  # Extract second named variable
  var.arr_2 <- var.get.nc(nc_2, var_2)

  # # NOTE - this is what i have done with cGENIE models.
  # # Is this the best way to deal with here,
  # # or just another way of translating to a nice grid?
  # # maybe for plotting either is kind of fine.
  # # but definitely would need to be fixed for point data matching.
  # # deal with weird lon coordinates if present
  # # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # amend HADCM3 grid to project on 0 degs
   if(mean(between(lon, -180, 180)) < 1){
     lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] - 360
     lon[lon >180] <- lon[lon >180] -360
   }

  # if(dims == 3){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      #as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))
      as.data.frame(melt(var.arr[,, depth.level]))$value,
      as.data.frame(melt(var.arr_2))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var",
                   "var_2"
    )
  # }
  # if(dims == 2){
  #   # generate dataframe of 2d genie slice from 3d genie array
  #   df <- as.data.frame(cbind(
  #     rep(lon, times = length(lat), each = 1),
  #     rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
  #     rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
  #     rep(lat, times = 1, each = length(lon)),
  #     rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
  #     rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
  #     #as.data.frame(melt(var.arr[,, time.step]))$value))
  #     as.data.frame(melt(var.arr[,, time.step]))$value))
  #
  #   names(df) <- c("lon.mid",
  #                  "lon.min",
  #                  "lon.max",
  #                  "lat.mid",
  #                  "lat.min",
  #                  "lat.max",
  #                  "var"
  #   )
  # }

    # eliminate cells outside of reasonable range
    df <- df %>%
      filter(lon.max <= 180,
             lon.min >= -180,
             lat.max <= 90,
             lat.min >= -90
             )

    # also eliminate cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
    df$lon.range <- abs(df$lon.min-df$lon.max)
    df <- df %>%
      filter(lon.range < 180 #could just be greater than 4, but this will work for all model grids
      )
    # currently have some issues with 0s in the orography raster.
    # for now just getting rid of them - later return to 0s vs 0.0s

    # currently assuming all layers have the same grid
    df_2 <- df
    df_2$var_2 <-as.factor(df_2$var_2)
    df_2 <- filter(df_2, var_2 != "0")
    df_2$var_2 <-as.numeric(paste(df_2$var_2))

    # something weird has happened where everything is flipped for the topography. Just flip it back?
    # could dig into at somepoint...
    df_2$lat.mid <- -df_2$lat.mid
    df_2$lat.min <- -df_2$lat.min
    df_2$lat.max <- -df_2$lat.max

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

  attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

  SpDf <- SpatialPolygonsDataFrame(SpP, attr)

  SpDfSf <- st_as_sf(SpDf)
  st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

  poly.list_2 <- list()
  poly.names.list_2 <- list()
  for(poly_2 in 1:(nrow(df_2))){

    polygon.code_2 <- Polygon(cbind(
      c(df_2$lon.min[poly_2], df_2$lon.max[poly_2], df_2$lon.max[poly_2], df_2$lon.min[poly_2]),
      c(df_2$lat.min[poly_2], df_2$lat.min[poly_2], df_2$lat.max[poly_2], df_2$lat.max[poly_2])))
    assign(paste0("Polygon_", poly_2), polygon.code_2)

    polygons.code_2 <- Polygons(list(polygon.code_2), paste0("p",poly_2))
    assign(paste0("Polygons_", poly_2), polygons.code_2)

    poly.list_2 <- append(poly.list_2, polygons.code_2)
    poly.names.list_2 <- append(poly.names.list_2, paste0("p",poly_2))
  }

  SpP_2 <- SpatialPolygons(poly.list_2)

  attr_2 <- data.frame(var = df_2$var_2, row.names = paste(poly.names.list_2))

  SpDf_2 <- SpatialPolygonsDataFrame(SpP_2, attr_2)

  SpDfSf_2 <- st_as_sf(SpDf_2)
  st_crs(SpDfSf_2) = '+proj=longlat +ellps=sphere'

  if(plot == FALSE){
    return(SpDfSf)
  }
  }
  if(plot == TRUE){
    if(calcs == FALSE){
      SpDfSf <- polygons
    }

  ## Outline of map using a framing line
  l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
  L1 <- Polygon(l1)
  Ls1 <- Polygons(list(L1), ID="a")
  SLs1 <-  SpatialPolygons(list(Ls1))

  df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
  names(df1)[1] <- "var"
  SLs1df = SpatialPolygonsDataFrame(SLs1, data = df1)
  SLs1dfSf <- st_as_sf(SLs1df)
  st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'


  min.value_1 <- 0
  max.value_1 <- 40
  intervals_1 <- 5

  min.value_2 <- 0
  max.value_2 <- 3200
  intervals_2 <- 400

  map <- ggplot() +
   geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    #geom_sf(data = SpDfSf_2 %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), fill=NA, color = "grey5", linewidth=0.9) +
    #coord_sf(crs = '+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs')+
    #coord_sf(crs = "ESRI:102003")+
    scale_fill_stepsn(colours = palette_name_ocean,
                      #scale_fill_stepsn(colours = parula(1000),# seems like we can keep the n value (1000) just at something big?
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 12,
                                             barheight = 1,
                                             raster = FALSE,
                                             frame.colour = "grey6",
                                             frame.linewidth = 2/.pt,
                                             frame.linetype = 1,
                                             ticks = TRUE,
                                             ticks.colour = "grey6",
                                             ticks.linewidth = 2/.pt),
                      breaks = seq(min.value_1, max.value_1, intervals_1),
                      limits=c(min.value_1, max.value_1),
                      #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
    )+
    theme(legend.position="bottom")+
    labs(fill = "Sea Surface Temperature (°C)")+
    new_scale_fill() +
    geom_sf(data = SpDfSf_2 %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    scale_fill_stepsn(colours = palette_name_land,
                      #scale_fill_stepsn(colours = parula(1000),# seems like we can keep the n value (1000) just at something big?
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 12,
                                             barheight = 1,
                                             raster = FALSE,
                                             frame.colour = "grey6",
                                             frame.linewidth = 2/.pt,
                                             frame.linetype = 1,
                                             ticks = TRUE,
                                             ticks.colour = "grey6",
                                             ticks.linewidth = 2/.pt),
                      breaks = seq(min.value_2, max.value_2, intervals_2),
                      limits=c(min.value_2, max.value_2),
                      #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
    )+
    theme_minimal()+
    theme(legend.position="bottom")+
    labs(fill = "Topography (m)")

  map
  }
}


