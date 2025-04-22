#' HADCM3.map
#'
#' This function generates maps from imported .nc files, such as those from Valdes et al. 2021. It can handle both 2D and 3D netCDF files and provides options for various customizations including projections, color palettes, and more.
#'
#' @param var Character. The variable to be extracted from the netCDF file.
#' @param file Character. The name of the netCDF file (without extension).
#' @param experiment Character. The path to the experiment directory containing the netCDF file.
#' @param depth.level Numeric. The depth level to be extracted for 3D netCDF files. Default is 1.
#' @param dims Numeric. The number of dimensions in the netCDF file (2 or 3). Default is 3.
#' @param min.value Numeric. The minimum value for the color scale.
#' @param max.value Numeric. The maximum value for the color scale.
#' @param intervals Numeric. The intervals for the color scale.
#' @param continents.outlined Logical. Whether to outline continents. (Not used in the current implementation)
#' @param scale.label Character. The label for the color scale.
#' @param unit.factor Numeric. A factor to multiply the variable values by. Default is 1.
#' @param time.present Logical. Whether the netCDF file includes a time dimension. Default is FALSE.
#' @param projection Character. The projection to be used for the map. Default is 'ESRI:54012'.
#' @param calcs Logical. Whether to perform calculations to generate the spatial polygons. Default is TRUE.
#' @param plot Logical. Whether to generate and return the plot. Default is TRUE.
#' @param palette_name Function. The color palette function to be used for the map. Default is `pals::parula(1000)`.
#' @param polygons SpatialPolygonsDataFrame. Pre-calculated polygons to be used if `calcs` is FALSE.
#' @param na.colour Character. The color to be used for NA values. Default is "grey80".
#' @param darkmode Logical. Whether to use dark mode for the plot. Default is FALSE.
#' @param bg.colour Character. Background color for the plot in dark mode. Default is "black".
#' @param fg.colour Character. Foreground color for the plot in dark mode. Default is "white".
#'
#' @return If `plot` is TRUE, returns a ggplot object. If `plot` is FALSE and `calcs` is TRUE, returns a SpatialPolygonsDataFrame. If `plot` is FALSE and `calcs` is FALSE, returns the input `polygons`.
#'
#' @import RNetCDF dplyr sf sp ggspatial reshape2 ggplot2 pals viridis
#' @export
HADCM3.map <- function(var, file, experiment,
      depth.level = 1,
      dims = 3,
       min.value,
       max.value,
       intervals,
       continents.outlined,
       scale.label,
       unit.factor = 1,
       time.present = FALSE,
       projection = 'ESRI:54012',
       calcs = TRUE,
       plot = TRUE,
       palette_name = pals::parula(1000),
       polygons,
       na.colour = "grey80",
       darkmode = FALSE,
       bg.colour = "black",
       fg.colour = "white"){

  # Open the netCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude values and calculate edges
  lat <- RNetCDF::var.get.nc(nc, "latitude")
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2))

  # Extract longitude values and calculate edges
  lon <- RNetCDF::var.get.nc(nc, "longitude")
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2))

  # If the netCDF file has 3 dimensions, extract depth values and calculate edges
  if(dims == 3){
    depth <- RNetCDF::var.get.nc(nc, "depth_1")
    depth.edges <- c(0, RNetCDF::var.get.nc(nc, "depth"), (depth[length(depth)]+307.5))
  }

  # If the netCDF file includes a time dimension, extract time values
  if(time.present == TRUE){
    time <- RNetCDF::var.get.nc(nc, "t")
  }

  # Extract the specified variable from the netCDF file
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # Amend HADCM3 grid to project on 0 degrees
  if(mean(dplyr::between(lon, -180, 180)) < 1){
    lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] - 360
    lon[lon >180] <- lon[lon >180] -360
  }

  # Generate dataframe of 2D slices from the 3D array
  if(dims == 3){
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(reshape2::melt(var.arr[,, depth.level]))$value))

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
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(reshape2::melt(var.arr))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )

    if(file == ".qrparm.orog" & var == "ht"){
      df$var <- as.factor(df$var)
      df <- dplyr::filter(df, var != "0")
      df$var <- as.numeric(paste(df$var))
    }
  }

  df <- df %>%
    dplyr::filter(lon.max <= 180,
                  lon.min >= -180,
                  lat.max <= 90,
                  lat.min >= -90,
                  lat.max >= -90,
                  lat.min <= 90
    )

  df$lon.range <- abs(df$lon.min - df$lon.max)
  df <- df %>%
    dplyr::filter(lon.range < 180)

  poly.list <- list()
  poly.names.list <- list()

  for(poly in 1:(nrow(df))){
    polygon.code <- sp::Polygon(cbind(
      c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
      c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
    assign(paste0("Polygon_", poly), polygon.code)

    polygons.code <- sp::Polygons(list(polygon.code), paste0("p", poly))
    assign(paste0("Polygons_", poly), polygons.code)

    poly.list <- append(poly.list, polygons.code)
    poly.names.list <- append(poly.names.list, paste0("p", poly))
  }

  SpP <- sp::SpatialPolygons(poly.list)
  attr <- data.frame(var = df$var, row.names = paste(poly.names.list))
  SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)
  SpDfSf <- sf::st_as_sf(SpDf)
  sf::st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

  l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
  L1 <- sp::Polygon(l1)
  Ls1 <- sp::Polygons(list(L1), ID="a")
  SLs1 <-  sp::SpatialPolygons(list(Ls1))

  df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
  names(df1)[1] <- "var"
  SLs1df = sp::SpatialPolygonsDataFrame(SLs1, data = df1)
  SLs1dfSf <- sf::st_as_sf(SLs1df)
  sf::st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = var * unit.factor), color = NA, linewidth = 10, linetype = 0) +
    ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = ifelse(darkmode, fg.colour, "grey5"), linewidth = 0.9, fill = NA) +
    ggplot2::scale_fill_stepsn(colours = palette_name,
              guide = ggplot2::guide_colorbar(title.position = "top",
                         barwidth = 12,
                         barheight = 1,
                         raster = FALSE,
                         frame.colour = ifelse(darkmode, fg.colour, "grey6"),
                         frame.linewidth = 2 / ggplot2::.pt,
                         frame.linetype = 1,
                         ticks = TRUE,
                         ticks.colour = ifelse(darkmode, fg.colour, "grey6"),
                         ticks.linewidth = 2 / ggplot2::.pt),
              breaks = seq(min.value, max.value, intervals),
              limits = c(min.value, max.value),
              na.value = na.colour
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
        plot.background = ggplot2::element_rect(fill = ifelse(darkmode, bg.colour, "white"), color = NA),
        panel.background = ggplot2::element_rect(fill = ifelse(darkmode, bg.colour, "white"), color = NA),
        legend.background = ggplot2::element_rect(fill = ifelse(darkmode, bg.colour, "white"), color = NA),
        text = ggplot2::element_text(color = ifelse(darkmode, fg.colour, "black")),
        axis.text = ggplot2::element_text(color = ifelse(darkmode, fg.colour, "black")),
        axis.title = ggplot2::element_text(color = ifelse(darkmode, fg.colour, "black")),
        legend.title = ggplot2::element_text(color = ifelse(darkmode, fg.colour, "black")),
        legend.text = ggplot2::element_text(color = ifelse(darkmode, fg.colour, "black"))
    ) +
    ggplot2::labs(fill = scale.label)

  map
}
