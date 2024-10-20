#' Generate Maps from cGENIE Model Output
#'
#' This function generates maps from imported .nc (NetCDF) files containing data from the cGENIE model outputs.
#' It can handle both 2D and 3D data, visualizing variables across specified depth levels and time steps.
#'
#' @param var (character) The variable from the NetCDF file to be visualized (e.g., "ocn_temp", "ocn_sal", "ocn_O2").
#' @param experiment (character) The path or name of the experiment used to locate the NetCDF file.
#' @param depth.level (numeric) Depth layer to visualize (default is 1 for the surface layer).
#' @param dims (numeric) The dimensionality of the data (default is 3 for 3D; can be 2D or 3D).
#' @param year (numeric or character) Time step to visualize (default uses the final time step if "default").
#' @param unit.factor (numeric) A scaling factor for the variable values (default is 1).
#' @param min.value (numeric) Minimum value for the color scale (used to set scale limits).
#' @param max.value (numeric) Maximum value for the color scale.
#' @param intervals (numeric) Step intervals for the color scale.
#' @param continents.outlined (logical) Logical value to control whether to outline continents.
#' @param scale.label (character) Label for the color bar.
#' @param model (character) The model type (default is 'biogem'; can be extended for other models).
#' @param palette_name (character) Color palette to be used for the plot (default is `pals::parula(1000)`).
#' @param projection (character) Map projection to use (default is ESRI:54012 for Equal Earth).
#'
#' @return A ggplot object representing the generated map with the specified variable visualized across geographical coordinates.
#'
#' @details
#' This function reads 2D or 3D data from cGENIE model output NetCDF files and produces a map visualization.
#' Default settings are defined for several commonly used variables, and users can specify their own scaling and color settings.
#'
#' @examples
#' map <- cGENIE.map(var = "ocn_temp", experiment = "my_experiment", depth.level = 1)
#' print(map)
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#' @export


cGENIE.map <- function(var, experiment,
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
                       projection = 'ESRI:54012') {

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

  # Add frame to the map
  l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180),
              c(-90, -90, seq(-90, 90, 0.1), 90, 90, seq(90, -90, -0.1), -90))
  L1 <- Polygon(l1)
  SLs1 <- SpatialPolygons(list(Polygons(list(L1), ID = "a")))
  SLs1df = SpatialPolygonsDataFrame(SLs1, data = data.frame(var = 2, row.names = "a"))
  SLs1dfSf <- st_as_sf(SLs1df)
  st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

  # Create the map using ggplot
  map <- ggplot() +
    geom_sf(data = SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA) +
    geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey5", linewidth = 0.9) +
    scale_fill_stepsn(colours = palette_name,
                      breaks = seq(min.value, max.value, intervals),
                      limits = c(min.value, max.value),
                      guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = scale.label)

  return(map)
}
