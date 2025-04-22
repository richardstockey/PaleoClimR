#' Generate Maps from cGENIE Model Output
#'
#' This function generates a specific kind of maps from imported .nc (NetCDF) files containing data from the cGENIE model outputs.
#' It plots OMZ depths based on some threshold.
#'
#' @param thresh A numeric value specifying the threshold oxygen concentration (in umol/kg) below which
#'                the volume is considered part of the OMZ. Default is 58.54e-6 based on Canfield 60uM
#'                https://enviromicro-journals.onlinelibrary.wiley.com/doi/10.1111/1462-2920.16192
#'                (just a conversion based on global mean density of 1.025 kg/L)
#' @param experiment (character) The path or name of the experiment used to locate the NetCDF file.
#' @param dims (numeric) The dimensionality of the data (default is 3 for 3D; can be 2D or 3D).
#' @param year (numeric or character) Time step to visualize (default uses the final time step if "default").
#' @param unit.factor (numeric) A scaling factor for the variable values (default is 1).
#' @param min.value (numeric) Minimum value for the color scale (used to set scale limits).
#' @param max.value (numeric) Maximum value for the color scale.
#' @param intervals (numeric) Step intervals for the color scale.
#' @param continents.outlined (logical) Logical value to control whether to outline continents.
#' @param line.thickness (numeric) Thickness of the lines outlining the continents and other map elements.
#' @param scale.label (character) Label for the color bar.
#' @param model (character) The model type (default is 'biogem'; can be extended for other models).
#' @param palette_name (character) Color palette to be used for the plot (default is `pals::parula(1000)`).
#' @param projection (character) Map projection to use (default is ESRI:54012 for Equal Earth).
#' @param darkmode (logical) Logical value to control whether to use dark mode (default is FALSE).
#' @param bg.color (character) Background color for the plot (default is "black" when darkmode is TRUE).
#' @param fg.color (character) Foreground color for the plot (default is "white" when darkmode is TRUE).
#'
#' @return A ggplot object representing the generated map with the specified variable visualized across geographical coordinates.
#'
#' @details
#' This function generates a map visualization of the global distribution of oxygen minimum zones from a given NetCDF file 
#' containing data from the cGENIE model outputs.
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#' @export
cGENIE.omz.map <- function(experiment,
         dims = 3,
         thresh = 58.54e-6,
         year = "default",
         unit.factor = NULL,
         min.value = NULL,
         max.value = NULL,
         intervals = NULL,
         continents.outlined = TRUE,
         line.thickness = 1,
         scale.label = NULL,
         model = "biogem",
         palette_name = pals::parula(1000),
         projection = 'ESRI:54012',
         darkmode = FALSE,
         bg.color = ifelse(darkmode, "black", "white"),
         fg.color = ifelse(darkmode, "white", "black")) {

  var <-  "ocn_O2"
  dims = 3
  unit.factor <- 1          # µmol/kg from mol/kg
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 5000, max.value)
  intervals <- ifelse(is.null(intervals), 500, intervals)
  scale.label <- ifelse(is.null(scale.label), "Thickness (m)", scale.label)

  depth.level = 1
  # Open the NetCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables (e.g., latitude, longitude, depth, time)
  lat <- RNetCDF::var.get.nc(nc, "lat") # Latitude
  lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")
  lon <- RNetCDF::var.get.nc(nc, "lon") # Longitude
  lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")
  depth <- RNetCDF::var.get.nc(nc, "zt") # Depth in meters
  depth.edges <- RNetCDF::var.get.nc(nc, "zt_edges")
  time <- RNetCDF::var.get.nc(nc, "time") # Time in years

  # Extract the specific variable from the NetCDF file
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # Set the time step to the final value if year is "default"
  if (year == "default") {
  time.step <- length(time)
  } else {
  time.step <- year
  }

  # Adjust longitude to be within 0 to 360 degrees (cGENIE model-specific)
  if (mean(dplyr::between(lon, -180, 180)) < 1) {
  lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
  lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Generate data frame for 3D data (if dims == 3)
  if (dims == 3) {
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat)),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
    rep(lon.edges[2:length(lon.edges)], times = length(lat)),
    rep(lat, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
    rep(lat.edges[2:length(lat.edges)], each = length(lon)),
    as.data.frame(reshape2::melt(var.arr[,, depth.level, time.step]))$value))
  names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  # Generate data frame for 2D data (if dims == 2)
  if (dims == 2) {
  if (var == "grid_topo") {
    df <- as.data.frame(cbind(
    rep(lon, times = length(lat)),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
    rep(lon.edges[2:length(lon.edges)], times = length(lat)),
    rep(lat, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
    rep(lat.edges[2:length(lat.edges)], each = length(lon)),
    as.data.frame(reshape2::melt(var.arr))$value))
    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  } else {
    df <- as.data.frame(cbind(
    rep(lon, times = length(lat)),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
    rep(lon.edges[2:length(lon.edges)], times = length(lat)),
    rep(lat, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
    rep(lat.edges[2:length(lat.edges)], each = length(lon)),
    as.data.frame(reshape2::melt(var.arr[,, time.step]))$value))
    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }
  }

  # Filter out invalid or extreme coordinate ranges
  df <- df %>%
  dplyr::filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

  # Handle longitudes near -180 and 180 degrees
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

  df2 <- df

  arr.3d <- cGENIE.data.3D(var = var, experiment = experiment,
               year = year,
               model = "biogem"
  )

  arr.3d.omz <- dplyr::filter(arr.3d, var <= thresh)

  arr.3d.omz$cell.thickness <- arr.3d.omz$depth.max - arr.3d.omz$depth.min

  lon.lat.omz <- arr.3d.omz %>%
  dplyr::group_by(lon.min, lon.max, lat.max, lat.min) %>%
  dplyr::summarise(var = sum(cell.thickness)) %>%
  as.data.frame()

  df <- lon.lat.omz
  # Create polygons for plotting
  poly.list <- list()
  poly.names.list <- list()
  for (poly in 1:nrow(df)) {
  polygon.code <- sp::Polygon(cbind(
    c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
    c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
  polygons.code <- sp::Polygons(list(polygon.code), paste0("p", poly))
  poly.list <- append(poly.list, polygons.code)
  poly.names.list <- append(poly.names.list, paste0("p", poly))
  }

  # Create spatial polygons data frame
  SpP <- sp::SpatialPolygons(poly.list)
  attr <- data.frame(var = df$var, row.names = poly.names.list)
  SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)
  SpDfSf <- sf::st_as_sf(SpDf)
  sf::st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

  # Add frame to the map
  l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180),
        c(-90, -90, seq(-90, 90, 0.1), 90, 90, seq(90, -90, -0.1), -90))
  L1 <- sp::Polygon(l1)
  SLs1 <- sp::SpatialPolygons(list(sp::Polygons(list(L1), ID = "a")))
  SLs1df = sp::SpatialPolygonsDataFrame(SLs1, data = data.frame(var = 2, row.names = "a"))
  SLs1dfSf <- sf::st_as_sf(SLs1df)
  sf::st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

  # Identify land cells (cells with NA values)
  land.cells <- arr.3d %>%
  dplyr::group_by(lon.min, lon.max, lat.max, lat.min) %>%
  dplyr::summarise(land = all(is.na(var))) %>%
  dplyr::filter(land == TRUE) %>%
  as.data.frame()

  # Create polygons for land cells
  land.poly.list <- list()
  land.poly.names.list <- list()
  for (poly in 1:nrow(land.cells)) {
  polygon.code <- sp::Polygon(cbind(
    c(land.cells$lon.min[poly], land.cells$lon.max[poly], land.cells$lon.max[poly], land.cells$lon.min[poly]),
    c(land.cells$lat.min[poly], land.cells$lat.min[poly], land.cells$lat.max[poly], land.cells$lat.max[poly])))
  polygons.code <- sp::Polygons(list(polygon.code), paste0("land", poly))
  land.poly.list <- append(land.poly.list, polygons.code)
  land.poly.names.list <- append(land.poly.names.list, paste0("land", poly))
  }

  # Create spatial polygons data frame for land cells
  LandSpP <- sp::SpatialPolygons(land.poly.list)
  land.attr <- data.frame(land = land.cells$land, row.names = land.poly.names.list)
  LandSpDf <- sp::SpatialPolygonsDataFrame(LandSpP, land.attr)
  LandSpDfSf <- sf::st_as_sf(LandSpDf)
  sf::st_crs(LandSpDfSf) = '+proj=longlat +ellps=sphere'

  if (continents.outlined == TRUE) {
  continent_polygons <- df2 %>% dplyr::filter(is.na(var))

  continent_polygons <- continent_polygons %>% dplyr::filter(!is.na(lon.min) & !is.na(lon.max) & !is.na(lat.min) & !is.na(lat.max))
  poly.list.continents <- list()
  for (poly in 1:(nrow(continent_polygons))) {
    polygon.code <- sp::Polygon(cbind(
    c(continent_polygons$lon.min[poly], continent_polygons$lon.max[poly], continent_polygons$lon.max[poly], continent_polygons$lon.min[poly]),
    c(continent_polygons$lat.min[poly], continent_polygons$lat.min[poly], continent_polygons$lat.max[poly], continent_polygons$lat.max[poly])))
    polygons.code <- sp::Polygons(list(polygon.code), paste0("p", poly))
    poly.list.continents <- append(poly.list.continents, polygons.code)
  }

  SpP.continents <- sp::SpatialPolygons(poly.list.continents)
  attr.continents <- data.frame(row.names = sapply(poly.list.continents, function(x) x@ID))
  SpDf.continents <- sp::SpatialPolygonsDataFrame(SpP.continents, attr.continents)
  SpDfSf.continents <- sf::st_as_sf(SpDf.continents)
  sf::st_crs(SpDfSf.continents) = '+proj=longlat +ellps=sphere'

  continents <- sf::st_union(SpDfSf.continents)

  # Create the map using ggplot with layered spatial objects
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = "grey20", linewidth = line.thickness, fill = "white") +
    ggplot2::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(fill = var * unit.factor), color = NA) +
    ggplot2::geom_sf(data = sf::st_as_sf(continents) %>% sf::st_transform(projection), fill = "grey80", color = "grey20", linewidth = line.thickness) +
    ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = "grey20", linewidth = line.thickness, fill = NA) +
    ggplot2::scale_fill_stepsn(colours = palette_name,
                 breaks = seq(min.value, max.value, intervals),
                 limits = c(min.value, max.value),
                 guide = ggplot2::guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
    ggplot2::theme_minimal(base_family = "Arial", base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
           plot.background = ggplot2::element_rect(fill = bg.color, color = NA),
           panel.background = ggplot2::element_rect(fill = bg.color, color = NA),
           panel.grid.major = ggplot2::element_line(color = fg.color),
           panel.grid.minor = ggplot2::element_line(color = fg.color),
           axis.text = ggplot2::element_text(color = fg.color),
           axis.title = ggplot2::element_text(color = fg.color),
           legend.text = ggplot2::element_text(color = fg.color),
           legend.title = ggplot2::element_text(color = fg.color)) +
    ggplot2::labs(fill = scale.label)

  } else {
  # Create the map using ggplot
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = "grey5", linewidth = 0.9, fill = "white") +
    ggplot2::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(fill = var * unit.factor), color = NA) +
    ggplot2::geom_sf(data = LandSpDfSf %>% sf::st_transform(projection), fill = "grey90", color = NA) +
    ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = "grey5", linewidth = 0.9, fill = NA) +
    ggplot2::scale_fill_stepsn(colours = palette_name,
                 breaks = seq(min.value, max.value, intervals),
                 limits = c(min.value, max.value),
                 guide = ggplot2::guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
    ggplot2::theme_minimal(base_family = "Arial", base_size = 12) +
    ggplot2::theme(legend.position = "bottom",
           plot.background = ggplot2::element_rect(fill = bg.color, color = NA),
           panel.background = ggplot2::element_rect(fill = bg.color, color = NA),
           panel.grid.major = ggplot2::element_line(color = fg.color),
           panel.grid.minor = ggplot2::element_line(color = fg.color),
           axis.text = ggplot2::element_text(color = fg.color),
           axis.title = ggplot2::element_text(color = fg.color),
           legend.text = ggplot2::element_text(color = fg.color),
           legend.title = ggplot2::element_text(color = fg.color)) +
    ggplot2::labs(fill = scale.label)
  }
  return(map)
}
