cGENIE.map.ortho <- function(var, experiment,
             depth.level = 1,
             dims = 3,
             year = "default",
             unit.factor = NULL,
             min.value = NULL,
             max.value = NULL,
             intervals = NULL,
             continents.outlined = TRUE,
             line.thickness = 0.5,
             line.colour = "grey20",
             scale.label = NULL,
             model = "biogem",
             palette_name = pals::parula(1000),
             projection = 'ESRI:54012',
             darkmode = FALSE,
             background.color = "black",
             text.color = "white") {

  # Load necessary libraries
  library(RNetCDF)   # For reading NetCDF files
  library(dplyr)     # For data manipulation
  library(sf)        # For handling spatial data
  library(sp)        # For working with spatial polygons
  library(ggspatial) # For adding spatial components in ggplot
  library(reshape2)  # For reshaping data
  library(ggplot2)   # For plotting

  # Define default values for different "var" variables
  if (var == "ocn_temp") {
  unit.factor <- 1          # no conversion
  dims = 3
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 40, max.value)
  intervals <- ifelse(is.null(intervals), 4, intervals)
  scale.label <- ifelse(is.null(scale.label), "Temperature (°C)", scale.label)
  } else if (var == "ocn_sal") {
  unit.factor <- 1          # no conversion
  dims = 3
  min.value <- ifelse(is.null(min.value), 30, min.value)
  max.value <- ifelse(is.null(max.value), 40, max.value)
  intervals <- ifelse(is.null(intervals), 1, intervals)
  scale.label <- ifelse(is.null(scale.label), "Salinity (PSU)", scale.label)
  } else if (var == "ocn_H2S") {
  dims = 3
  unit.factor <- 1e6          # µmol/kg from mol/kg
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 40, max.value)
  intervals <- ifelse(is.null(intervals), 4, intervals)
  scale.label <- ifelse(is.null(scale.label), expression(H[2]*S ~ (mu*mol/kg)), scale.label)
  } else if (var == "ocn_O2") {
  dims = 3
  unit.factor <- 1e6          # µmol/kg from mol/kg
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 300, max.value)
  intervals <- ifelse(is.null(intervals), 25, intervals)
  scale.label <- ifelse(is.null(scale.label), "Oxygen (µmol/kg)", scale.label)
  } else if (var == "grid_topo") {
  unit.factor <- -1          # no conversion
  dims = 2
  min.value <- ifelse(is.null(min.value), -5000, min.value)
  max.value <- ifelse(is.null(max.value), 0, max.value)
  intervals <- ifelse(is.null(intervals), 500, intervals)
  scale.label <- ifelse(is.null(scale.label), expression("Ocean Depth (m)"), scale.label)
  } else if (var == "phys_psi") {
  unit.factor <- 1          # no conversion
  dims = 2
  min.value <- ifelse(is.null(min.value), -75, min.value)
  max.value <- ifelse(is.null(max.value), 75, max.value)
  intervals <- ifelse(is.null(intervals), 15, intervals)
  scale.label <- ifelse(is.null(scale.label), "Barotropic streamfunction (Sv)", scale.label)
  } else {
  # Default values for any other variable
  unit.factor <- 1          # default, no conversion
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 100, max.value)
  intervals <- ifelse(is.null(intervals), 10, intervals)
  scale.label <- ifelse(is.null(scale.label), "Variable", scale.label)
  }

  # Set model-specific file prefix
  if (model == "biogem") {
  prefix <- "/biogem/fields_biogem_"
  }

  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables (e.g., latitude, longitude, depth, time)
  lat <- var.get.nc(nc, "lat") # Latitude
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon") # Longitude
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt") # Depth in meters
  depth.edges <- var.get.nc(nc, "zt_edges")
  time <- var.get.nc(nc, "time") # Time in years

  # Extract the specific variable from the NetCDF file
  var.arr <- var.get.nc(nc, var)

  # Set the time step to the final value if year is "default"
  if (year == "default") {
  time.step <- length(time)
  } else {
  time.step <- year
  }

  # Adjust longitude to be within 0 to 360 degrees (cGENIE model-specific)
  if (mean(between(lon, -180, 180)) < 1) {
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
    as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))
  names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  # Generate data frame for 2D data (if dims == 2)
  if (dims == 2) {
  if(var == "grid_topo"){
    df <- as.data.frame(cbind(
    rep(lon, times = length(lat)),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
    rep(lon.edges[2:length(lon.edges)], times = length(lat)),
    rep(lat, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
    rep(lat.edges[2:length(lat.edges)], each = length(lon)),
    as.data.frame(melt(var.arr))$value))
    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }else{
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat)),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
    rep(lon.edges[2:length(lon.edges)], times = length(lat)),
    rep(lat, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
    rep(lat.edges[2:length(lat.edges)], each = length(lon)),
    as.data.frame(melt(var.arr[,, time.step]))$value))
  names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }
  }

  # Filter out invalid or extreme coordinate ranges
  df <- df %>%
  filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

  # Handle longitudes near -180 and 180 degrees
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

  # Create polygons for plotting
  poly.list <- list()
  poly.names.list <- list()
  for (poly in 1:nrow(df)) {
  polygon.code <- Polygon(cbind(
    c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
    c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
  polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
  poly.list <- append(poly.list, polygons.code)
  poly.names.list <- append(poly.names.list, paste0("p", poly))
  }

  # Create spatial polygons data frame
  SpP <- SpatialPolygons(poly.list)
  attr <- data.frame(var = df$var, row.names = poly.names.list)
  SpDf <- SpatialPolygonsDataFrame(SpP, attr)
  SpDfSf <- st_as_sf(SpDf)
  st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

  # Transform SpDfSf for orthogonal projection
  SpDfSf_transformed <- SpDfSf %>%
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split = TRUE) %>%
  st_transform(crs = projection) %>%
  mutate(npts = npts(geometry, by_feature = TRUE)) %>%
  filter(npts > 3) %>%
  st_cast('POLYGON', group_or_split = TRUE)

  if (continents.outlined == TRUE) {
  continent_polygons <- df %>% filter(is.na(var))

  poly.list.continents <- list()
  for (poly in 1:(nrow(continent_polygons))) {
    polygon.code <- Polygon(cbind(
    c(continent_polygons$lon.min[poly], continent_polygons$lon.max[poly], continent_polygons$lon.max[poly], continent_polygons$lon.min[poly]),
    c(continent_polygons$lat.min[poly], continent_polygons$lat.min[poly], continent_polygons$lat.max[poly], continent_polygons$lat.max[poly])))
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    poly.list.continents <- append(poly.list.continents, polygons.code)
  }

  SpP.continents <- SpatialPolygons(poly.list.continents)
  attr.continents <- data.frame(row.names = sapply(poly.list.continents, function(x) x@ID))
  SpDf.continents <- SpatialPolygonsDataFrame(SpP.continents, attr.continents)
  SpDfSf.continents <- st_as_sf(SpDf.continents)
  st_crs(SpDfSf.continents) = '+proj=longlat +ellps=sphere'

  continents <- st_union(SpDfSf.continents)

  # Transform SpDfSf for orthogonal projection
  continents_transformed <- continents %>%
    st_cast('MULTILINESTRING') %>%
    st_cast('LINESTRING', do_split = TRUE) %>%
    st_transform(crs = projection) %>%
    st_sf() %>%
    mutate(npts = npts(geometry, by_feature = TRUE)) %>%
    filter(npts > 3) %>%
    st_cast('POLYGON')

  map <- ggplot() +
    geom_sf(data = SpDfSf_transformed, aes(fill = var * unit.factor), color = NA) +
    geom_sf(data = continents_transformed, fill = "grey80", color = line.colour, linewidth = line.thickness) +
    scale_fill_stepsn(colours = palette_name,
            breaks = seq(min.value, max.value, intervals),
            limits = c(min.value, max.value),
            guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
      plot.background = element_rect(fill = ifelse(darkmode, background.color, "white")),
      panel.background = element_rect(fill = ifelse(darkmode, background.color, "white")),
      text = element_text(color = ifelse(darkmode, text.color, "black"))) +
    labs(fill = scale.label) +
    coord_sf(crs = st_crs(projection))


  }else{
  # Create the map using ggplot
  map <- ggplot() +
    geom_sf(data = SpDfSf_transformed, aes(fill = var * unit.factor), color = NA) +
    scale_fill_stepsn(colours = palette_name,
            na.value = "grey70",
            breaks = seq(min.value, max.value, intervals),
            limits = c(min.value, max.value),
            guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
      plot.background = element_rect(fill = ifelse(darkmode, background.color, "white")),
      panel.background = element_rect(fill = ifelse(darkmode, background.color, "white")),
      text = element_text(color = ifelse(darkmode, text.color, "black"))) +
    labs(fill = scale.label) +
    coord_sf(crs = st_crs(projection))


  }


  return(map)
}
