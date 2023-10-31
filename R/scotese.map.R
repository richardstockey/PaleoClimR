###################################################
# scotese.map.R
# Rich Stockey 20231030
# designed to make maps from imported .nc files (from https://zenodo.org/records/5460860)
###################################################
# full comments to follow...

scotese.map <- function(map, # netcdf file
                       min.value = -6000,
                       max.value = 6000,
                       intervals = 2000,
                       continents.outlined,
                       scale.label,
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

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "lon") # units: degrees
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  z <- var.get.nc(nc, "z") # units: metres

    # generate dataframe
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(melt(z)$value)))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "z"
    )

    df <- df %>%
      filter(lon.max < 180,
             lon.min > -180,
             lat.max < 90,
             lat.min > -90
             )


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

  attr <- data.frame(var = df$z, row.names = paste(poly.names.list))

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

  mapplot <- ggplot() +
    geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), fill=NA, color = "grey5", linewidth=0.9) +
    #coord_sf(crs = '+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs')+
    #coord_sf(crs = "ESRI:102003")+
    scale_fill_binned(type = scale,
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

  mapplot
}


