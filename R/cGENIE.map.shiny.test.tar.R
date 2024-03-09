###################################################
# cGENIE.map.R
# Rich Stockey 20230922
# designed to make maps from imported .nc files (imported separately from cGENIE.nc.import.R because of hyperdimensionality) [?]
###################################################
# full comments to follow...

cGENIE.map.shiny.test.tar <- function(file, var, name,
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
  library(stringr)
  # name <- "~/PETM.stuff/cgenie_archive/S22_056a_Tdep_remin_1PO4.tar.gz"

  file.name.short <- name %>% str_replace(".tar.gz", "")

  if(dims == 3){
  nc <- open.nc(archive::archive_extract(file, files = c(paste0(file.name.short, "/biogem/fields_biogem_3d.nc"))))
  }
  if(dims == 2){
    nc <- open.nc(archive::archive_extract(file, files = c(paste0(file.name.short, "/biogem/fields_biogem_2d.nc"))))
  }
  # tar <- archive_read(file, "biogem/fields_biogem_3d.nc")
  #
  # nc <- open.nc(archive_read(file, "S22_056a_Tdep_remin_1PO4/biogem/fields_biogem_3d.nc"))
  #
  #
  # read.csv(archive_read(file, "S22_056a_Tdep_remin_1PO4/biogem/biogem_series_atm_temp.res"))

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

  if(year == "default"){
    time.step <- length(time)
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
    # generate dataframe of 2d genie slice from 3d genie array
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
  }

  # eliminate cells outside of reasonable range
  df <- df %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90
    )

  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df$lon.range <- abs(df$lon.min-df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

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


