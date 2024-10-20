#' HADCM3.m2.to.absolute.map
#'
#' This function is designed to upscale per area estimates of variables such as TRIFFID outputs to global values.
#'
#' @param var Character. The name of the variable to extract from the NetCDF file.
#' @param file Character. The name of the NetCDF file (without extension).
#' @param experiment Character. The path to the experiment directory containing the NetCDF file.
#' @param depth.level Numeric. The depth level to extract (default is 1).
#' @param dims Numeric. The dimensions of the NetCDF file (default is 2).
#' @param min.value Numeric. The minimum value for the color scale.
#' @param max.value Numeric. The maximum value for the color scale.
#' @param intervals Numeric. The intervals for the color scale.
#' @param continents.outlined Logical. Whether to outline continents (default is FALSE).
#' @param scale.label Character. The label for the color scale.
#' @param unit.factor Numeric. A factor to scale the variable values (default is 1).
#' @param time.present Logical. Whether the time dimension is present in the NetCDF file (default is FALSE).
#' @param projection Character. The projection to use for the map (default is 'ESRI:54012').
#' @param calcs Logical. Whether to perform calculations (default is TRUE).
#' @param plot Logical. Whether to generate a plot (default is TRUE).
#' @param palette_name Character. The name of the color palette to use (default is pals::parula(1000)).
#' @param polygons List. A list of polygons to overlay on the map.
#' @param na.colour Character. The color to use for NA values (default is "grey80").
#'
#' @return A ggplot object representing the map.
#' @import RNetCDF dplyr sf sp reshape2 ggplot2 pals geosphere
#' @export
#'
#' @examples
#' \dontrun{
#' HADCM3.m2.to.absolute.map(var = "insitu_T_ym_dpth", file = "o.pgclann", experiment = "~/Valdes2021_HADCM3L/teXPl_444/teXPl_444",
#'                           min.value = 0, max.value = 100, intervals = 10, continents.outlined = TRUE, scale.label = "Temperature",
#'                           unit.factor = 1, time.present = FALSE, projection = 'ESRI:54012', calcs = TRUE, plot = TRUE,
#'                           palette_name = pals::parula(1000), polygons = NULL, na.colour = "grey80")
#' }
HADCM3.m2.to.absolute.map <- function(var, file, experiment,
      depth.level = 1,
      dims = 2,
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
       na.colour = "grey80"){

  # Load necessary libraries
  library(RNetCDF)  # For handling NetCDF files
  library(dplyr)    # For data manipulation
  library(sf)       # For handling spatial data
  library(sp)       # For spatial data classes and methods
  library(reshape2) # For reshaping data
  library(ggplot2)  # For plotting
  library(pals)     # For color palettes
  library(geosphere) # For geospatial calculations
  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract latitude and longitude variables
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  lon <- var.get.nc(nc, "longitude") # units: degrees east

  # Calculate the edges of latitude and longitude for plotting
  # This assumes that the grid is evenly spaced
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid

  # If the NetCDF file has 3 dimensions, extract the depth variable
  if(dims == 3){
    depth <- var.get.nc(nc, "depth_1") # units: metres
    # Calculate the edges of depth for plotting
    depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres
  }

  # If the time dimension is present, extract the time variable
  if(time.present == TRUE){
    time <- var.get.nc(nc, "t") # units: year mid-point
  }
  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  # NOTE - this is what i have done with cGENIE models.
  # Is this the best way to deal with here,
  # or just another way of translating to a nice grid?
  # maybe for plotting either is kind of fine.
  # but definitely would need to be fixed for point data matching.
  # deal with weird lon coordinates if present
  # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }
  # Amend HADCM3 grid to project on 0 degrees
  if(mean(between(lon, -180, 180)) < 1){
    # Adjust longitude edges and values to be within the range of -180 to 180 degrees
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges > 180] - 360
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  if(dims == 3){
    # Generate dataframe of 2D slice from 3D array for the specified depth level
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),  # Repeat longitude values for each latitude
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges for each latitude
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges for each latitude
      rep(lat, times = 1, each = length(lon)),  # Repeat latitude values for each longitude
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges for each longitude
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges for each longitude
      as.data.frame(melt(var.arr[,, depth.level]))$value  # Extract variable values for the specified depth level
    ))

    # Assign column names to the dataframe
    names(df) <- c("lon.mid",  # Midpoint of longitude
                   "lon.min",  # Minimum longitude edge
                   "lon.max",  # Maximum longitude edge
                   "lat.mid",  # Midpoint of latitude
                   "lat.min",  # Minimum latitude edge
                   "lat.max",  # Maximum latitude edge
                   "var"  # Variable values
    )
  }

  if(dims == 2){
    # Generate dataframe of 2D slice from 2D array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),  # Repeat longitude values for each latitude
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges for each latitude
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges for each latitude
      rep(lat, times = 1, each = length(lon)),  # Repeat latitude values for each longitude
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges for each longitude
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges for each longitude
      as.data.frame(melt(var.arr))$value  # Extract variable values
    ))

    # Assign column names to the dataframe
    names(df) <- c("lon.mid",  # Midpoint of longitude
                   "lon.min",  # Minimum longitude edge
                   "lon.max",  # Maximum longitude edge
                   "lat.mid",  # Midpoint of latitude
                   "lat.min",  # Minimum latitude edge
                   "lat.max",  # Maximum latitude edge
                   "var"  # Variable values
    )
  }

  # Eliminate cells outside of reasonable range
  df <- df %>%
    filter(lon.max <= 180,  # Filter out cells with longitude max greater than 180 degrees
           lon.min >= -180,  # Filter out cells with longitude min less than -180 degrees
           lat.max <= 90,  # Filter out cells with latitude max greater than 90 degrees
           lat.min >= -90,  # Filter out cells with latitude min less than -90 degrees
           lat.max >= -90,  # Ensure latitude max is not less than -90 degrees
           lat.min <= 90  # Ensure latitude min is not greater than 90 degrees
    )

  # NOTE - NOT SURE I HAVE A PERFECT UNDERSTANDING OF THE HADCM3 grid geometry but this should be pretty much correct as looks fine in map view
  # Sit down with BRIDGE group at somepoint?

  # SCRAPPED THIS FOR TOTAL CALCULATIONS...
  # also eliminate cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  # Calculate the range of longitude for each cell
  df$lon.range <- abs(df$lon.min - df$lon.max)
  
  # Filter out cells that span more than 180 degrees in longitude
  df <- df %>%
    filter(lon.range < 180) # This will work for all model grids

  # Initialize lists to store polygons and their names
  poly.list <- list()
  poly.names.list <- list()
  
  # Loop through each row in the dataframe to create polygons
  for(poly in 1:(nrow(df))){
    # Create a polygon for each cell using the longitude and latitude edges
    polygon.code <- Polygon(cbind(
      c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
      c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])
    ))
    assign(paste0("Polygon_", poly), polygon.code)

    # Create a Polygons object from the polygon
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    assign(paste0("Polygons_", poly), polygons.code)

    # Append the Polygons object and its name to the lists
    poly.list <- append(poly.list, polygons.code)
    poly.names.list <- append(poly.names.list, paste0("p", poly))
  }

  # Create a SpatialPolygons object from the list of Polygons
  SpP <- SpatialPolygons(poly.list)

  # Calculate the area of each polygon in square meters
  area_m2 <- areaPolygon(SpP)

  # Add the area as a new column to the dataframe
  df <- cbind(df, area_m2)

  # Calculate the total value for each cell by multiplying the variable value by the area
  df$TotalCellVal <- df$var * df$area_m2

  # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
  attr <- data.frame(var = df$TotalCellVal, row.names = paste(poly.names.list))

  # Create a SpatialPolygonsDataFrame from the SpatialPolygons and attributes
  SpDf <- SpatialPolygonsDataFrame(SpP, attr)

  # Convert the SpatialPolygonsDataFrame to an sf object
  SpDfSf <- st_as_sf(SpDf)
  st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

  ## Outline of map using a framing line
  l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), 
              c(-90, -90, seq(-90, 90, 0.1), 90, 90, seq(90, -90, -0.1), -90))
  L1 <- Polygon(l1)
  Ls1 <- Polygons(list(L1), ID="a")
  SLs1 <- SpatialPolygons(list(Ls1))

  # Create a dataframe for the outline attributes
  df1 <- data.frame(rep(2, 1), row.names = rep("a", 1))
  names(df1)[1] <- "var"
  SLs1df = SpatialPolygonsDataFrame(SLs1, data = df1)
  SLs1dfSf <- st_as_sf(SLs1df)
  st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

  # Create the map using ggplot2
  map <- ggplot() +
    geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill = var * unit.factor), 
            color = NA, linewidth = 10, linetype = 0) + # WGS 84 / Equal Earth Greenwich
    geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), 
            fill = NA, color = "grey5", linewidth = 0.9) +
    scale_fill_stepsn(colours = palette_name,
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 12,
                                             barheight = 1,
                                             raster = FALSE,
                                             frame.colour = "grey6",
                                             frame.linewidth = 2 / .pt,
                                             frame.linetype = 1,
                                             ticks = TRUE,
                                             ticks.colour = "grey6",
                                             ticks.linewidth = 2 / .pt),
                      breaks = seq(min.value, max.value, intervals),
                      limits = c(min.value, max.value),
                      na.value = na.colour) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = scale.label)

  # Return the map
  map

}


