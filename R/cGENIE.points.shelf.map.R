#' Generate Maps from cGENIE Model Output with Points Matching
#'
#' This function generates maps from imported .nc (NetCDF) files containing data from the cGENIE model outputs.
#' It can handle both 2D and 3D data, visualizing variables across specified depth levels and time steps.
#' Additionally, it matches and plots specific points from a provided data frame.
#'
#' @param var (character) The variable from the NetCDF file to be visualized (e.g., "ocn_temp", "ocn_sal", "ocn_O2").
#' @param experiment (character) The path or name of the experiment used to locate the NetCDF file.
#' @param input (array) The input data array for the cGENIE model.
#' @param format (character) The format of the input data (default is "nc").
#' @param depth.level (numeric) Depth layer to visualize (default is 1 for the surface layer).
#' @param dims (numeric) The dimensionality of the data (default is 3 for 3D; can be 2D or 3D).
#' @param ocean.alpha (numeric) Alpha transparency for ocean areas in the plot (default is 0.4).
#' @param year (numeric or character) Time step to visualize (default uses the final time step if "default").
#' @param unit.factor (numeric) A scaling factor for the variable values (default is 1).
#' @param min.value (numeric) Minimum value for the color scale (used to set scale limits).
#' @param max.value (numeric) Maximum value for the color scale.
#' @param intervals (numeric) Step intervals for the color scale.
#' @param continents.outlined (logical) Logical value to control whether to outline continents.
#' @param scale.label (character) Label for the color bar.
#' @param model (character) The model type (default is 'biogem'; can be extended for other models).
#' @param palette_name (character) Color palette to be used for the plot (default is 'pals::parula(1000)').
#' @param projection (character) Map projection to use (default is ESRI:54012 for Equal Earth).
#' @param line.thickness (numeric) Thickness of the lines outlining continents (default is 1).
#' @param coord.dat (data.frame) Data frame with latitude and longitude columns to which cGENIE data will be added and returned.
#' @param lat.name (character) Name of the latitude column in `coord.dat` (default is "p_lat").
#' @param lng.name (character) Name of the longitude column in `coord.dat` (default is "p_lng").
#' @param darkmode (logical) Logical value to control whether to use dark mode (default is FALSE).
#' @param bg.colour (character) Background color for the map in dark mode (default is "black").
#' @param fg.colour (character) Foreground color for the map in dark mode (default is "white").
#'
#' @return A ggplot object representing the generated map with the specified variable visualized across geographical coordinates.
#'
#' @details
#' This function reads 2D or 3D data from cGENIE model output NetCDF files and produces a map visualization.
#' Default settings are defined for several commonly used variables, and users can specify their own scaling and color settings.
#' The function also matches specific points from a provided data frame and plots them on the map.
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#' @export

cGENIE.points.shelf.map <- function(var = NULL,
                experiment = NULL,
                input = NULL,
                format = "nc",
                depth.level = 1,
                dims = 3,
                ocean.alpha = 0.4,
                year = "default",
                unit.factor = NULL,
                min.value = NULL,
                max.value = NULL,
                intervals = NULL,
                continents.outlined = TRUE,
                scale.label = NULL,
                model = "biogem",
                palette_name = pals::parula(1000),
                projection = 'ESRI:54012',
                line.thickness = 1,
                coord.dat = NULL, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                lng.name = "p_lng", # name IF generated from rotated paleoverse coordinates...
                darkmode = FALSE,
                bg.colour = "black",
                fg.colour = "white") # dark mode options
{


  if(format == "nc"){
  matched_points <- cGENIE.point.matching(var = var,
                                          experiment = experiment,
                                          depth.level = depth.level,
                                          dims = dims,
                                          coord.dat = coord.dat,
                                          lat.name = lat.name,
                                          lng.name = lng.name
  )

  # Define default values for different "var" variables
  if (var == "ocn_temp") {
    unit.factor <- 1          # no conversion
    dims = 3
    min.value <- ifelse(is.null(min.value), 0, min.value)
    max.value <- ifelse(is.null(max.value), 40, max.value)
    intervals <- ifelse(is.null(intervals), 4, intervals)
    scale.label <- ifelse(is.null(scale.label), "Temperature (\u00B0C)", scale.label)
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
    scale.label <- ifelse(is.null(scale.label), "Hydrogen Sulfide (\u03BCmol/kg)", scale.label)
  } else if (var == "ocn_O2") {
  dims = 3
  unit.factor <- 1e6          # µmol/kg from mol/kg
  min.value <- ifelse(is.null(min.value), 0, min.value)
  max.value <- ifelse(is.null(max.value), 300, max.value)
  intervals <- ifelse(is.null(intervals), 25, intervals)
  scale.label <- ifelse(is.null(scale.label), "Oxygen (\u00B5mol/kg)", scale.label)
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


  # Create spatial polygons for NA values (land)
  land.df <- df %>% filter(is.na(var))
  land.poly.list <- list()
  land.poly.names.list <- list()
  for (poly in 1:nrow(land.df)) {
    polygon.code <- Polygon(cbind(
      c(land.df$lon.min[poly], land.df$lon.max[poly], land.df$lon.max[poly], land.df$lon.min[poly]),
      c(land.df$lat.min[poly], land.df$lat.min[poly], land.df$lat.max[poly], land.df$lat.max[poly])))
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    land.poly.list <- append(land.poly.list, polygons.code)
    land.poly.names.list <- append(land.poly.names.list, paste0("p", poly))
  }

  # Create spatial polygons data frame for land data
  land.SpP <- SpatialPolygons(land.poly.list)
  land.attr <- data.frame(var = land.df$var, row.names = land.poly.names.list)
  land.SpDf <- SpatialPolygonsDataFrame(land.SpP, land.attr)
  land.SpDfSf <- st_as_sf(land.SpDf)
  st_crs(land.SpDfSf) = '+proj=longlat +ellps=sphere'

  # Run the var.arr through cGENIE.shelf to get the shelf array
  shelf.arr <- cGENIE.shelf(input = var.arr[,,, time.step], format = "array")

  # Generate data frame for shelf data (if dims == 3)
  if (dims == 3) {
    shelf.df <- as.data.frame(cbind(
      rep(lon, times = length(lat)),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
      rep(lon.edges[2:length(lon.edges)], times = length(lat)),
      rep(lat, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], each = length(lon)),
      as.data.frame(melt(shelf.arr[,, depth.level]))$value))
    names(shelf.df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  # Generate data frame for shelf data (if dims == 2)
  if (dims == 2) {
    shelf.df <- as.data.frame(cbind(
      rep(lon, times = length(lat)),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
      rep(lon.edges[2:length(lon.edges)], times = length(lat)),
      rep(lat, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], each = length(lon)),
      as.data.frame(melt(shelf.arr[,, time.step]))$value))
    names(shelf.df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
  }

  # Filter out invalid or extreme coordinate ranges for shelf data
  shelf.df <- shelf.df %>%
    filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

  # Handle longitudes near -180 and 180 degrees for shelf data
  shelf.df$lon.range <- abs(shelf.df$lon.min - shelf.df$lon.max)
  shelf.df$lon.min[shelf.df$lon.range > 180 & abs(shelf.df$lon.min) == 180] <- -shelf.df$lon.min[shelf.df$lon.range > 180 & abs(shelf.df$lon.min) == 180]
  shelf.df$lon.max[shelf.df$lon.range > 180 & abs(shelf.df$lon.max) == 180] <- -shelf.df$lon.max[shelf.df$lon.range > 180 & abs(shelf.df$lon.max) == 180]

  # Create polygons for shelf data
  shelf.poly.list <- list()
  shelf.poly.names.list <- list()
  for (poly in 1:nrow(shelf.df)) {
    polygon.code <- Polygon(cbind(
      c(shelf.df$lon.min[poly], shelf.df$lon.max[poly], shelf.df$lon.max[poly], shelf.df$lon.min[poly]),
      c(shelf.df$lat.min[poly], shelf.df$lat.min[poly], shelf.df$lat.max[poly], shelf.df$lat.max[poly])))
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    shelf.poly.list <- append(shelf.poly.list, polygons.code)
    shelf.poly.names.list <- append(shelf.poly.names.list, paste0("p", poly))
  }

  # Create spatial polygons data frame for shelf data
  shelf.SpP <- SpatialPolygons(shelf.poly.list)
  shelf.attr <- data.frame(var = shelf.df$var, row.names = shelf.poly.names.list)
  shelf.SpDf <- SpatialPolygonsDataFrame(shelf.SpP, shelf.attr)
  shelf.SpDfSf <- st_as_sf(shelf.SpDf)
  st_crs(shelf.SpDfSf) = '+proj=longlat +ellps=sphere'

  # Remove NAs from shelf data
  shelf.SpDfSf <- shelf.SpDfSf %>% filter(!is.na(var))

  matched_points$lat <- matched_points$p_lat
  matched_points$lng <- matched_points$p_lng

  # Create spatial object with the chosen points from start of script
  points <- as.data.frame(cbind(matched_points$lng, matched_points$lat, matched_points$matched_climate*unit.factor))
  points <- na.omit(points)
  points_sp <- SpatialPointsDataFrame(coords = points[,1:2], data = as.data.frame(points[,3]))
  names(points_sp) <- "matched_climate"

  # code in colour scale for matched points
  palette_name_points <- palette_name
  min.value_1 <- min.value
  max.value_1 <- max.value
  intervals_1 <- intervals

  # make plottable object
  points_spsf <- st_as_sf(points_sp)
  st_crs(points_spsf) = '+proj=longlat +ellps=sphere'

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

    # Create the map using ggplot
    map <- ggplot() +
      geom_sf(data = SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA, alpha = ocean.alpha) +
      geom_sf(data = shelf.SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA) +
      geom_sf(data = land.SpDfSf %>% st_transform(projection), fill = "grey80", color = NA) +
      geom_sf(data = st_as_sf(continents) %>% st_transform(projection), fill = "grey80", color = "grey20", linewidth = line.thickness)+
    geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey20", linewidth = line.thickness, fill = NA) +
      scale_fill_stepsn(colours = palette_name,
                        breaks = seq(min.value, max.value, intervals),
                        limits = c(min.value, max.value),
                        guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = scale.label)
  }else{
    # Create the map using ggplot
    map <- ggplot() +
      geom_sf(data = SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA, alpha = ocean.alpha) +
      geom_sf(data = shelf.SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA) +
      geom_sf(data = land.SpDfSf %>% st_transform(projection), fill = "grey80", color = NA) +
      geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey20", linewidth = line.thickness, fill = NA) +
      scale_fill_stepsn(colours = palette_name,
                        breaks = seq(min.value, max.value, intervals),
                        limits = c(min.value, max.value),
                        guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = scale.label)
  }
  map.points <- map +
    geom_sf(data = points_spsf %>% st_transform(projection), aes(geometry = geometry, fill = matched_climate), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

  return(map.points)

  }else if(format == "array"){ # for now, this is just copied over from my current version of cGENEI.poin.matching...
    # if using an array – use default cGENIE dims
    grid.dat <- list(
      lat = c(-76.463797, -66.443536, -59.441568, -53.663942, -48.590378, -43.982963,
              -39.709017, -35.685335, -31.855431, -28.178643, -24.624318, -21.168449,
              -17.791591, -14.477512, -11.212271,  -7.983556,  -4.780192,  -1.591754,
              1.591754,   4.780192,   7.983556,  11.212271,  14.477512,  17.791591,
              21.168449,  24.624318,  28.178643,  31.855431,  35.685335,  39.709017,
              43.982963,  48.590378,  53.663942,  59.441568,  66.443536,  76.463797),
      lat.edges = c(
        -90.000000, -70.811864, -62.733956, -56.442690, -51.057559, -46.238257,
        -41.810315, -37.669887, -33.748989, -30.000000, -26.387800, -22.885380,
        -19.471221, -16.127620, -12.839588,  -9.594068,  -6.379370,  -3.184739,
        0.000000,   3.184739,   6.379370,   9.594068,  12.839588,  16.127620,
        19.471221,  22.885380,  26.387800,  30.000000,  33.748989,  37.669887,
        41.810315,  46.238257,  51.057559,  56.442690,  62.733956,  70.811864,
        90.000000),
      lon = c(-175, -165, -155, -145, -135, -125, -115, -105,  -95,  -85,  -75,  -65,  -55,  -45,  -35,
              -25,  -15,   -5,    5,   15,   25,   35,   45,   55,   65,   75,   85,   95,  105,  115,
              125,  135,  145,  155,  165,  175),
      lon.edges = c(
        -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60, -50, -40,
        -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110,
        120, 130, 140, 150, 160, 170, 180),
      depth = c(  40.42035,  127.55154,  228.77023,  346.35409,  482.94908,  641.62894,
                  825.96438, 1040.10344, 1288.86481, 1577.84627, 1913.55066, 2303.53222,
                  2756.56654, 3282.84810, 3894.21960, 4604.43852),
      depth.edges = c(   0.00000,   80.84071,  174.75186,  283.84670,  410.58014,  557.80403,
                         728.83129,  927.51048, 1158.31240, 1426.43070, 1737.89874, 2099.72539,
                         2520.05268, 3008.33908, 3575.57232, 4234.51663, 5000.00000)
    )

    # then, use key elements of cGENIE.data to extract the same data format from the array
    var.arr <- input
    # Extract general variables
    lat <- grid.dat$lat            # Latitude (degrees north)
    lat.edges <- grid.dat$lat.edges # Latitude edges
    lon <- grid.dat$lon            # Longitude (degrees east)
    lon.edges <- grid.dat$lon.edges # Longitude edges

    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
      as.data.frame(melt(var.arr[,, depth.level]))$value
    ))

    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")

    # Filter the data to ensure valid lat-lon ranges
    df <- df %>%
      filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    # Handle cells at the extremes (-180 and 180 longitude)
    df$lon.range <- abs(df$lon.min - df$lon.max)
    df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
    df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

    clim.dat <- df


    # Omit NAs in the var value for climate data file
    # This step filters out any rows in the climate data where the specified variable has NA values.
    # This ensures that only valid data points are used i§n the matching process.
    clim.dat <- filter(clim.dat, !is.na(var))
    # Remove any NA paleocoordinates
    # This step filters out any rows in the coordinate data where the latitude or longitude values are NA.
    # This ensures that only valid coordinates are used in the matching process.
    coord.dat <- filter(coord.dat, !is.na(!!sym(lng.name)) & !is.na(!!sym(lat.name)))

    # Initialize a column for matched climate data
    # This step adds a new column to the coordinate data frame to store the matched climate data.
    # Initially, all values in this column are set to NA.
    coord.dat$matched_climate <- NA

    # Iterate over each row in the coordinate data frame
    # This loop processes each coordinate point to find the matching climate data.
    for(row in 1:nrow(coord.dat)){

      # Find the mid-point of the nearest latitudinal grid cell for each occurrence
      # This step identifies the closest latitude grid point in the cGENIE model to the current coordinate's latitude.
      coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat[[lat.name]][row] - grid.dat$lat))]

      # Identify all the cells in the climate model that have the same latitude as the data point
      # This step filters the climate data to include only the rows where the latitude matches the closest latitude grid point.
      lat.mid.opts <- clim.dat %>%
        filter(lat.mid == coord.dat$lat.bin.mid[row])

      # Check if there are any matching latitude options and if the closest longitudinal bin is within 10 degrees
      # This condition ensures that there are valid latitude matches and that the closest longitude grid point is within a reasonable distance.
      if(nrow(lat.mid.opts) > 0 & min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid)) < 10){

        # Find the mid-point of the nearest longitudinal grid cell
        # This step identifies the closest longitude grid point in the cGENIE model to the current coordinate's longitude.
        coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid))]

        # Assign the matched climate data based on the assigned latitudinal and longitudinal bins
        # This step retrieves the climate data value for the closest latitude and longitude grid points and assigns it to the matched_climate column.
        coord.dat$matched_climate[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row]]

      } else {
        # If there are no valid latitude matches or the nearest longitude grid cell is more than 10 degrees away, assign NA
        # This step handles cases where the coordinate point is too far from any valid climate data points.
        coord.dat$lon.bin.mid[row] <- NA
        coord.dat$matched_climate[row] <- NA
      }
    }

    # Filter out rows where the matched climate data is NA
    # This step removes any coordinate points that did not have valid climate data matches.
    coord.dat <- filter(coord.dat, is.na(matched_climate) == FALSE)
    matched_points <- coord.dat

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


    # Create spatial polygons for NA values (land)
    land.df <- df %>% filter(is.na(var))
    land.poly.list <- list()
    land.poly.names.list <- list()
    for (poly in 1:nrow(land.df)) {
      polygon.code <- Polygon(cbind(
        c(land.df$lon.min[poly], land.df$lon.max[poly], land.df$lon.max[poly], land.df$lon.min[poly]),
        c(land.df$lat.min[poly], land.df$lat.min[poly], land.df$lat.max[poly], land.df$lat.max[poly])))
      polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
      land.poly.list <- append(land.poly.list, polygons.code)
      land.poly.names.list <- append(land.poly.names.list, paste0("p", poly))
    }

    # Create spatial polygons data frame for land data
    land.SpP <- SpatialPolygons(land.poly.list)
    land.attr <- data.frame(var = land.df$var, row.names = land.poly.names.list)
    land.SpDf <- SpatialPolygonsDataFrame(land.SpP, land.attr)
    land.SpDfSf <- st_as_sf(land.SpDf)
    st_crs(land.SpDfSf) = '+proj=longlat +ellps=sphere'

    # Run the var.arr through cGENIE.shelf to get the shelf array
    shelf.arr <- cGENIE.shelf(input = var.arr, format = "array")

    # Generate data frame for shelf data (if dims == 3)
    if (dims == 3) {
      shelf.df <- as.data.frame(cbind(
        rep(lon, times = length(lat)),
        rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
        rep(lon.edges[2:length(lon.edges)], times = length(lat)),
        rep(lat, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
        rep(lat.edges[2:length(lat.edges)], each = length(lon)),
        as.data.frame(melt(shelf.arr[,, depth.level]))$value))
      names(shelf.df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    }

    # Generate data frame for shelf data (if dims == 2)
    if (dims == 2) {
      shelf.df <- as.data.frame(cbind(
        rep(lon, times = length(lat)),
        rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat)),
        rep(lon.edges[2:length(lon.edges)], times = length(lat)),
        rep(lat, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges)-1)], each = length(lon)),
        rep(lat.edges[2:length(lat.edges)], each = length(lon)),
        as.data.frame(melt(shelf.arr[,, time.step]))$value))
      names(shelf.df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    }

    # Filter out invalid or extreme coordinate ranges for shelf data
    shelf.df <- shelf.df %>%
      filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    # Handle longitudes near -180 and 180 degrees for shelf data
    shelf.df$lon.range <- abs(shelf.df$lon.min - shelf.df$lon.max)
    shelf.df$lon.min[shelf.df$lon.range > 180 & abs(shelf.df$lon.min) == 180] <- -shelf.df$lon.min[shelf.df$lon.range > 180 & abs(shelf.df$lon.min) == 180]
    shelf.df$lon.max[shelf.df$lon.range > 180 & abs(shelf.df$lon.max) == 180] <- -shelf.df$lon.max[shelf.df$lon.range > 180 & abs(shelf.df$lon.max) == 180]

    # Create polygons for shelf data
    shelf.poly.list <- list()
    shelf.poly.names.list <- list()
    for (poly in 1:nrow(shelf.df)) {
      polygon.code <- Polygon(cbind(
        c(shelf.df$lon.min[poly], shelf.df$lon.max[poly], shelf.df$lon.max[poly], shelf.df$lon.min[poly]),
        c(shelf.df$lat.min[poly], shelf.df$lat.min[poly], shelf.df$lat.max[poly], shelf.df$lat.max[poly])))
      polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
      shelf.poly.list <- append(shelf.poly.list, polygons.code)
      shelf.poly.names.list <- append(shelf.poly.names.list, paste0("p", poly))
    }

    # Create spatial polygons data frame for shelf data
    shelf.SpP <- SpatialPolygons(shelf.poly.list)
    shelf.attr <- data.frame(var = shelf.df$var, row.names = shelf.poly.names.list)
    shelf.SpDf <- SpatialPolygonsDataFrame(shelf.SpP, shelf.attr)
    shelf.SpDfSf <- st_as_sf(shelf.SpDf)
    st_crs(shelf.SpDfSf) = '+proj=longlat +ellps=sphere'

    # Remove NAs from shelf data
    shelf.SpDfSf <- shelf.SpDfSf %>% filter(!is.na(var))

    matched_points$lat <- matched_points$p_lat
    matched_points$lng <- matched_points$p_lng

    # Create spatial object with the chosen points from start of script
    points <- as.data.frame(cbind(matched_points$lng, matched_points$lat, matched_points$matched_climate*unit.factor))
    points <- na.omit(points)
    points_sp <- SpatialPointsDataFrame(coords = points[,1:2], data = as.data.frame(points[,3]))
    names(points_sp) <- "matched_climate"

    # code in colour scale for matched points
    palette_name_points <- palette_name
    min.value_1 <- min.value
    max.value_1 <- max.value
    intervals_1 <- intervals

    # make plottable object
    points_spsf <- st_as_sf(points_sp)
    st_crs(points_spsf) = '+proj=longlat +ellps=sphere'

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

        # Create the map using ggplot
      map <- ggplot() +
        geom_sf(data = SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA, alpha = ocean.alpha) +
        geom_sf(data = shelf.SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA) +
        geom_sf(data = land.SpDfSf %>% st_transform(projection), fill = "grey80", color = NA) +
        geom_sf(data = st_as_sf(continents) %>% st_transform(projection), fill = "grey80", color = "grey20", linewidth = line.thickness)+
      geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey20", linewidth = line.thickness, fill = NA) +
        scale_fill_stepsn(colours = palette_name,
              breaks = seq(min.value, max.value, intervals),
              limits = c(min.value, max.value),
              guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
        theme_minimal() +
        theme(legend.position = "bottom",
          panel.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          plot.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          legend.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          axis.text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          axis.title = element_text(color = ifelse(darkmode, fg.colour, "black")),
          legend.text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          legend.title = element_text(color = ifelse(darkmode, fg.colour, "black"))) +
        labs(fill = scale.label)
        }else{
        # Create the map using ggplot
      map <- ggplot() +
        geom_sf(data = SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA, alpha = ocean.alpha) +
        geom_sf(data = shelf.SpDfSf %>% st_transform(projection), aes(fill = var * unit.factor), color = NA) +
        geom_sf(data = land.SpDfSf %>% st_transform(projection), fill = "grey80", color = NA) +
      geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey20", linewidth = line.thickness, fill = NA) +
        scale_fill_stepsn(colours = palette_name,
              breaks = seq(min.value, max.value, intervals),
              limits = c(min.value, max.value),
              guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 1)) +
        theme_minimal() +
        theme(legend.position = "bottom",
          panel.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          plot.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          legend.background = element_rect(fill = ifelse(darkmode, bg.colour, "white")),
          text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          axis.text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          axis.title = element_text(color = ifelse(darkmode, fg.colour, "black")),
          legend.text = element_text(color = ifelse(darkmode, fg.colour, "black")),
          legend.title = element_text(color = ifelse(darkmode, fg.colour, "black"))) +
        labs(fill = scale.label)
    }

        map.points <- map +
      geom_sf(data = points_spsf %>% st_transform(projection), aes(geometry = geometry, fill = matched_climate), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

        return(map.points)
  }
}
