#' Overlay Map for cGENIE Data
#'
#' This function creates an overlay map for cGENIE data using various projections and dimensions.
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param model The model type; defaults to "biogem".
#' @param min.value Minimum value for the color scale. Defaults to NULL.
#' @param max.value Maximum value for the color scale. Defaults to NULL.
#' @param intervals Number of intervals for the color scale. Defaults to NULL.
#' @param scale.label Label for the color scale. Defaults to NULL.
#' @return A ggplot object representing the overlay map.
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @importFrom sf st_as_sf
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom ggspatial annotation_map_tile
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn
#' @export
#' @examples
#' \dontrun{
#' overlay_map <- cGENIE.overlay.map(var = "temperature", experiment = "experiment1")
#' }

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

  # Load necessary libraries
  library(RNetCDF)   # For reading and manipulating netCDF files
  library(dplyr)     # For data manipulation using the pipe operator (%>%)
  library(sf)        # For handling spatial data using simple features
  library(sp)        # For spatial data classes and methods
  library(ggspatial) # For adding spatial layers to ggplot2
  library(reshape2)  # For reshaping data
  library(ggplot2)   # For creating plots using the grammar of graphics

  # Set default values based on the variable
  if (var == "phys_psi") {
  unit.factor <- 1          # no conversion
  dims <- 2
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

  # Determine the prefix based on the model type
  if(model == "biogem"){
  prefix <- "/biogem/fields_biogem_"
  }

  # Open the netCDF file corresponding to the experiment and model type
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

  # Extract oxygen variable arbitrarily to get land mask
  oxy.arr <- var.get.nc(nc, "ocn_sur_O2")

  # Determine the time step to use
  if(year == "default"){
  time.step <- length(time)  # Use the last time step if year is set to default
  } else {
  time.step <- year  # Use the specified year as the time step
  }

  # Adjust longitude values to be within the range 0 to 360 degrees
  if(mean(between(lon, -180, 180)) < 1){
  lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
  lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  if(dims == 3){
  # Generate a dataframe for a 2D slice from a 3D array
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),  # Repeat longitude values
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min)
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max)
    rep(lat, times = 1, each = length(lon)),  # Repeat latitude values
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min)
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max)
    as.data.frame(melt(var.arr[,, depth.level, time.step]))$value  # Extract variable values for the specified depth and time step
  ))

  # Assign column names to the dataframe
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
  # Generate a dataframe for a 2D array
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),  # Repeat longitude values
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min)
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max)
    rep(lat, times = 1, each = length(lon)),  # Repeat latitude values
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min)
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max)
    as.data.frame(melt(var.arr[,, time.step]))$value  # Extract variable values for the specified time step
  ))

  # Assign column names to the dataframe
  names(df) <- c("lon.mid",
           "lon.min",
           "lon.max",
           "lat.mid",
           "lat.min",
           "lat.max",
           "var"
  )
  }

  # Create a dataframe for the oxygen variable to get the land mask
  df2 <- as.data.frame(cbind(
  rep(lon, times = length(lat), each = 1),  # Repeat longitude values
  rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min)
  rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max)
  rep(lat, times = 1, each = length(lon)),  # Repeat latitude values
  rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min)
  rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max)
  as.data.frame(melt(oxy.arr[,, time.step]))$value  # Extract oxygen values for the specified time step
  ))

  # Assign column names to the dataframe
  names(df2) <- c("lon.mid",
          "lon.min",
          "lon.max",
          "lat.mid",
          "lat.min",
          "lat.max",
          "oxy"
  )


  # Eliminate cells outside of the reasonable range for the main variable dataframe
  df <- df %>%
  filter(lon.max <= 180,  # Filter out cells with longitude max greater than 180
       lon.min >= -180,  # Filter out cells with longitude min less than -180
       lat.max <= 90,  # Filter out cells with latitude max greater than 90
       lat.min >= -90  # Filter out cells with latitude min less than -90
  )

  # Eliminate cells outside of the reasonable range AND only select NA cells for the oxygen dataframe
  df2 <- df2 %>%
  filter(lon.max <= 180,  # Filter out cells with longitude max greater than 180
       lon.min >= -180,  # Filter out cells with longitude min less than -180
       lat.max <= 90,  # Filter out cells with latitude max greater than 90
       lat.min >= -90,  # Filter out cells with latitude min less than -90
       is.na(oxy)  # Only select cells where oxygen value is NA
  )
  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df2$lon.range <- abs(df2$lon.min - df2$lon.max)
  df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180] <- -df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180]
  df2$lon.max[df2$lon.range > 180 & abs(df2$lon.max) == 180] <- -df2$lon.max[df2$lon.range > 180 & abs(df2$lon.max) == 180]

  # Create a list to store polygons for the main variable dataframe
  poly.list <- list()
  poly.names.list <- list()
  for (poly in 1:(nrow(df))) {
    # Create a polygon for each cell in the dataframe
    polygon.code <- Polygon(cbind(
    c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
    c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])
    ))
    assign(paste0("Polygon_", poly), polygon.code)

    # Create a Polygons object for each polygon
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    assign(paste0("Polygons_", poly), polygons.code)

    # Append the Polygons object to the list
    poly.list <- append(poly.list, polygons.code)
    poly.names.list <- append(poly.names.list, paste0("p", poly))
  }

  # Create a SpatialPolygons object from the list of Polygons objects
  SpP <- SpatialPolygons(poly.list)

  # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
  attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

  # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the attributes dataframe
  SpDf <- SpatialPolygonsDataFrame(SpP, attr)

  # Convert the SpatialPolygonsDataFrame to an sf object
  SpDfSf <- st_as_sf(SpDf)
  st_crs(SpDfSf) <- '+proj=longlat +ellps=sphere'

  # Create polygons for NA cells (land mask)
  poly2.list <- list()
  poly2.names.list <- list()
  for(poly2 in 1:(nrow(df2))){
    # Create a polygon for each cell in the dataframe
    polygon.code2 <- Polygon(cbind(
    c(df2$lon.min[poly2], df2$lon.max[poly2], df2$lon.max[poly2], df2$lon.min[poly2]),
    c(df2$lat.min[poly2], df2$lat.min[poly2], df2$lat.max[poly2], df2$lat.max[poly2])
    ))
    assign(paste0("polygon_", poly2), polygon.code2)

    # Create a Polygons object for each polygon
    polygons.code2 <- Polygons(list(polygon.code2), paste0("p", poly2))
    assign(paste0("polygons_", poly2), polygons.code2)

    # Append the Polygons object to the list
    poly2.list <- append(poly2.list, polygons.code2)
    poly2.names.list <- append(poly2.names.list, paste0("p", poly2))
  }

  # Create a SpatialPolygons object from the list of Polygons objects
  SpP2 <- SpatialPolygons(poly2.list)

  # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
  attr2 <- data.frame(var = df2$oxy, row.names = paste(poly2.names.list))

  # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the attributes dataframe
  Spdf2 <- SpatialPolygonsDataFrame(SpP2, attr2)

  # Convert the SpatialPolygonsDataFrame to an sf object
  Spdf2Sf <- st_as_sf(Spdf2)
  st_crs(Spdf2Sf) <- '+proj=longlat +ellps=sphere'

  # Create an outline of the map using a framing line
  l1 <- cbind(
    c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180),
    c(-90, -90, seq(-90, 90, 0.1), 90, 90, seq(90, -90, -0.1), -90)
  )
  L1 <- Polygon(l1)
  Ls1 <- Polygons(list(L1), ID = "a")
  SLs1 <- SpatialPolygons(list(Ls1))

  # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
  df1 <- data.frame(rep(2, 1), row.names = rep("a", 1))
  names(df1)[1] <- "var"
  SLs1df <- SpatialPolygonsDataFrame(SLs1, data = df1)

  # Convert the SpatialPolygonsDataFrame to an sf object
  SLs1dfSf <- st_as_sf(SLs1df)
  st_crs(SLs1dfSf) <- '+proj=longlat +ellps=sphere'

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


