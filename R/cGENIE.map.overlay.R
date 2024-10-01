###################################################
# cGENIE.map.overlay.R
# Rich Stockey 20240904
# designed to add continment overlay to stream function plots
###################################################
# full comments to follow...
# NOTE - for now only doing this for 2D biogem files as its the only file type i need it for

cGENIE.overlay.map <- function(var, experiment,
                       depth.level = 1,
                       dims = 3,
                       year = "default",
                       unit.factor = 1,
                       min.value,
                       max.value,
                       intervals,
                       continents.outlined,
                       scale.label,
                       model = "biogem",
                       palette_name = pals::parula(1000),
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
  oxy.arr <- var.get.nc(nc, "ocn_sur_O2")

  if(year == "default"){
    time.step <- length(time)
  }else{
    time.step <- year
  }

  # amend grid to project on 0 degs - note cGENIE differs from HADCM3
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= - 180] <- lon.edges[lon.edges <= - 180] + 360
    lon[lon <= - 180] <- lon[lon <= - 180] + 360
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
      as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))

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
    # generate dataframe of 2d genie slice from 2d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[,, time.step]))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )

    df2 <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(oxy.arr[,, time.step]))$value))

    names(df2) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "oxy"
    )
  }

  # eliminate cells outside of reasonable range
  df <- df %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90
    )

  # eliminate cells outside of reasonable range AND only select NA cells
  df2 <- df2 %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90,
           is.na(oxy)
    )

  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df$lon.range <- abs(df$lon.min-df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df2$lon.range <- abs(df2$lon.min-df2$lon.max)
  df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180] <- -df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180]
  df2$lon.max[df2$lon.range > 180 & abs(df2$lon.max) == 180] <- -df2$lon.max[df2$lon.range > 180 & abs(df2$lon.max) == 180]

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

  # plot.new()

  # Make NA polys
  poly2.list <- list()
  poly2.names.list <- list()
  for(poly2 in 1:(nrow(df2))){

    polygon.code2 <- Polygon(cbind(
      c(df2$lon.min[poly2], df2$lon.max[poly2], df2$lon.max[poly2], df2$lon.min[poly2]),
      c(df2$lat.min[poly2], df2$lat.min[poly2], df2$lat.max[poly2], df2$lat.max[poly2])))
    assign(paste0("polygon_", poly2), polygon.code2)

    polygons.code2 <- Polygons(list(polygon.code2), paste0("p",poly2))
    assign(paste0("polygons_", poly2), polygons.code2)

    poly2.list <- append(poly2.list, polygons.code2)
    poly2.names.list <- append(poly2.names.list, paste0("p",poly2))
  }

  SpP2 <- SpatialPolygons(poly2.list)

  attr2 <- data.frame(var = df2$oxy, row.names = paste(poly2.names.list))

  Spdf2 <- SpatialPolygonsDataFrame(SpP2, attr2)

  Spdf2Sf <- st_as_sf(Spdf2)
  st_crs(Spdf2Sf) = '+proj=longlat +ellps=sphere'

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

  map <- ggplot() +
    geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill=var*unit.factor), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), fill=NA, color = "grey5", linewidth=0.9) +
    geom_sf(data = Spdf2Sf %>% st_transform(projection), aes(geometry = geometry), fill = "grey80", alpha = 1, color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    #coord_sf(crs = '+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs')+
    #coord_sf(crs = "ESRI:102003")+
    scale_fill_stepsn(colours = palette_name,
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
                      breaks = seq(min.value, max.value, intervals),
                      limits=c(min.value, max.value),
                      #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
    )+
    theme_minimal()+
    theme(legend.position="bottom")+
    labs(fill = scale.label)

  map
}


