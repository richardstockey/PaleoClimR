#' HADCM3.map.pretty
#'
#' This function generates visually appealing maps from imported .nc files such as those from Valdes et al. 2021. It aims to include key information to visualize as much of the earth system as possible at one time.
#'
#' @param experiment A string specifying the path to the experiment directory.
#' @param depth.level An integer specifying the depth level to be visualized. Default is 1.
#' @param dims An integer specifying the dimensions of the netCDF file being read in. Default is 3.
#' @param land.opt A string specifying land options. Default is "default".
#' @param min.value Minimum value for the color scale.
#' @param max.value Maximum value for the color scale.
#' @param intervals Intervals for the color scale.
#' @param continents.outlined Logical indicating whether continents should be outlined.
#' @param scale.label Label for the color scale.
#' @param unit.factor A numeric factor to adjust units. Default is 1.
#' @param time.present Logical indicating whether time is present in the data. Default is FALSE.
#' @param scale A string specifying the color scale to use. Default is "viridis".
#' @param projection A string specifying the map projection to use. Default is 'ESRI:54012'.
#' @param calcs Logical indicating whether calculations should be performed. Default is TRUE.
#' @param plot Logical indicating whether to plot the map. Default is TRUE.
#' @param for.app Logical indicating whether the map is for an app. Default is FALSE.
#' @param polygons Spatial polygons data frame if calcs is FALSE.
#' @param darkmode Logical indicating whether to use dark mode. Default is FALSE.
#' @param bg.color Background color for the plot. Default is "white".
#' @param fg.color Foreground color for the plot elements. Default is "black".
#' @param file Name of the netCDF file (without extension). This should generally be ".qrparm".
#' @param var Name of the variable to extract from the netCDF file. Default is "insitu_T_ym_dpth".
#' @param file_2 Name of the second netCDF file (without extension). should generally be ".qrparm.orog"
#' @param palette_name_ocean Color palette for ocean data visualization. Default is pals::parula(1000).
#' @param col.labels Labels for the color scale. Default is NULL.
#'
#' @return If plot is FALSE - returns a spatial polygons data frame. If plot is TRUE - returns a ggplot object.
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf st_transform st_crs 
#' @importFrom ggspatial geom_sf annotation_scale
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes labs theme
#' @importFrom ggnewscale new_scale 
#' @importFrom paletteer paletteer_c
#' @export
#'

HADCM3.map.pretty <- function(file = NULL,  # Name of the netCDF file (without extension)
  var = NULL,  # Name of the variable to extract from the netCDF file
  experiment = NULL,
  file_2 = NULL,
  depth.level = 1,
  dims = 3,
  land.opt = "default",
  min.value = NULL,
  max.value = NULL,
  intervals = NULL,
  continents.outlined = NULL,
  scale.label = NULL,
  unit.factor = 1,
  time.present = FALSE,
  scale = "viridis",
  projection = 'ESRI:54012',
  calcs = TRUE,
  plot = TRUE,
  for.app = FALSE,
  palette_name_ocean = pals::parula(1000),
  polygons = NULL,
  darkmode = FALSE,
  bg.color = "white",
  fg.color = "black",
  col.labels = NULL) {

  # Define the color palette for land data visualization
  palette_name_land <- paletteer::paletteer_c("grDevices::Light Grays", 30)  # Using the 'Light Grays' color palette from the 'grDevices' package with 30 color steps

  # Conditional statement to open the netCDF file if calculations are to be performed
  if(calcs == TRUE){
    # Construct the full path to the netCDF file by concatenating the experiment path and file name
    nc <- RNetCDF::open.nc(paste0(experiment, file, ".nc"))
  }

  # Extract general variables
  # NOTE - these may not precisely represent the HADCM3 grid
  # fudged slightly for ease of plotting from the xxx and xxx_1 variables.
  # worth checking with HADCM3 users to be doubly sure
  # this should be kept separate from variable matching scripts with point data such as PBDB and therefore be functionally fine
  lat <- RNetCDF::var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- RNetCDF::var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

  if(dims == 3){
    depth <- RNetCDF::var.get.nc(nc, "depth_1") # units: metres
  depth.edges <- c(0, RNetCDF::var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep
  }
  if(time.present == TRUE){
  time <- RNetCDF::var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
  # note that not all of these general variables will be available for fields_biogem_2d (address later)
  }

  # Extract named variable from the netCDF file
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # Now we're going to load a second netCDF file with orography values
  # Define the file name and variable name for the second netCDF file
  # file_2 <- ".qrparm.orog"  # Name of the second netCDF file (without extension)
  var_2 <- "ht"  # Name of the variable to extract from the second netCDF file

  # Open the second netCDF file by constructing the full path using the experiment path and file name
  nc_2 <- RNetCDF::open.nc(paste0(experiment, file_2, ".nc"))

  # Extract the second named variable from the second netCDF file
  var.arr_2 <- RNetCDF::var.get.nc(nc_2, var_2)

  # # NOTE - this is what i have done with cGENIE models.
  # # Is this the best way to deal with here,
  # # or just another way of translating to a nice grid?
  # # maybe for plotting either is kind of fine.
  # # but definitely would need to be fixed for point data matching.
  # # deal with weird lon coordinates if present
  # # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(dplyr::between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # Amend HADCM3 grid to project on 0 degrees
  # Check if longitude values are within the range -180 to 180
  if(mean(dplyr::between(lon, -180, 180)) < 1){
    # Adjust longitude edges and values greater than 180 by subtracting 360 to bring them within the range
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges > 180] - 360
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  if(dims == 3){
  # Generate dataframe of 2D slice from 3D array
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),  # Repeat longitude values for each latitude
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min) for each latitude
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max) for each latitude
    rep(lat, times = 1, each = length(lon)),  # Repeat latitude values for each longitude
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min) for each longitude
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max) for each longitude
    as.data.frame(reshape2::melt(var.arr[,, depth.level]))$value,  # Melt the 3D array to get variable values at the specified depth level
    as.data.frame(reshape2::melt(var.arr_2))$value  # Melt the second variable array to get its values
  ))
  }
  if(dims == 2){
    # use 2d array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),  # Repeat longitude values for each latitude
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min) for each latitude
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max) for each latitude
      rep(lat, times = 1, each = length(lon)),  # Repeat latitude values for each longitude
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min) for each longitude
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max) for each longitude
      as.data.frame(reshape2::melt(var.arr))$value,  # Melt the 3D array to get variable values at the specified depth level
      as.data.frame(reshape2::melt(var.arr_2))$value  # Melt the second variable array to get its values
    ))
  }
  # Assign column names to the dataframe
  names(df) <- c("lon.mid",  # Midpoint longitude
                 "lon.min",  # Minimum longitude edge
                 "lon.max",  # Maximum longitude edge
                 "lat.mid",  # Midpoint latitude
                 "lat.min",  # Minimum latitude edge
                 "lat.max",  # Maximum latitude edge
                 "var",  # Variable values from the first netCDF file
                 "var_2"  # Variable values from the second netCDF file
  )


    # Eliminate cells outside of the reasonable range for longitude and latitude
    df <- df %>%
      dplyr::filter(lon.max <= 180,  # Ensure maximum longitude is within 180 degrees
         lon.min >= -180, # Ensure minimum longitude is within -180 degrees
         lat.max <= 90,   # Ensure maximum latitude is within 90 degrees
         lat.min >= -90   # Ensure minimum latitude is within -90 degrees
         )

    # Eliminate cells that bridge the left and right sides of the map (i.e., extreme -180ish and 180ish longitude)
    df$lon.range <- abs(df$lon.min - df$lon.max)  # Calculate the range of longitude for each cell
    df <- df %>%
      dplyr::filter(lon.range < 180 # Filter out cells with a longitude range greater than or equal to 180 degrees
      )

    # Currently have some issues with 0s in the orography raster.
    # For now, just getting rid of them - later return to 0s vs 0.0s
    # Assuming all layers have the same grid
    df_2 <- df  # Create a copy of the dataframe for the second variable
    df_2$var_2 <- as.factor(df_2$var_2)  # Convert the second variable to a factor
    df_2 <- dplyr::filter(df_2, var_2 != "0")  # Filter out cells where the second variable is 0
    df_2$var_2 <- as.numeric(paste(df_2$var_2))  # Convert the second variable back to numeric

    # Something weird has happened where everything is flipped for the topography. Just flip it back?
    # Could dig into this at some point...
    df_2$lat.mid <- -df_2$lat.mid  # Flip the midpoint latitude values
    df_2$lat.min <- -df_2$lat.min  # Flip the minimum latitude values
    df_2$lat.max <- -df_2$lat.max  # Flip the maximum latitude values
    # Initialize lists to store polygons and their names for the first variable
    poly.list <- list()
    poly.names.list <- list()

    # Loop through each row in the dataframe to create polygons
    for(poly in 1:(nrow(df))){
      # Create a polygon using the longitude and latitude edges
      polygon.code <- sp::Polygon(cbind(
        c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
        c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
      assign(paste0("Polygon_", poly), polygon.code)

      # Create a Polygons object from the polygon
      polygons.code <- sp::Polygons(list(polygon.code), paste0("p",poly))
      assign(paste0("Polygons_", poly), polygons.code)

      # Append the Polygons object and its name to the respective lists
      poly.list <- append(poly.list, polygons.code)
      poly.names.list <- append(poly.names.list, paste0("p",poly))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP <- sp::SpatialPolygons(poly.list)

    # Set values of var in df that equal -99999.00 to NA
    df$var[df$var == -99999.00] <- NA

    # Create a dataframe with the variable values and row names
    attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the dataframe
    SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)

    # Convert the SpatialPolygonsDataFrame to an sf object and set the coordinate reference system
    SpDfSf <- sf::st_as_sf(SpDf)
    sf::st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

    # Initialize lists to store polygons and their names for the second variable
    poly.list_2 <- list()
    poly.names.list_2 <- list()

    # Loop through each row in the dataframe for the second variable to create polygons
    for(poly_2 in 1:(nrow(df_2))){
      # Create a polygon using the longitude and latitude edges
      polygon.code_2 <- sp::Polygon(cbind(
        c(df_2$lon.min[poly_2], df_2$lon.max[poly_2], df_2$lon.max[poly_2], df_2$lon.min[poly_2]),
        c(df_2$lat.min[poly_2], df_2$lat.min[poly_2], df_2$lat.max[poly_2], df_2$lat.max[poly_2])))
      assign(paste0("Polygon_", poly_2), polygon.code_2)

      # Create a Polygons object from the polygon
      polygons.code_2 <- sp::Polygons(list(polygon.code_2), paste0("p",poly_2))
      assign(paste0("Polygons_", poly_2), polygons.code_2)

      # Append the Polygons object and its name to the respective lists
      poly.list_2 <- append(poly.list_2, polygons.code_2)
      poly.names.list_2 <- append(poly.names.list_2, paste0("p",poly_2))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP_2 <- sp::SpatialPolygons(poly.list_2)

    # Create a dataframe with the variable values and row names
    attr_2 <- data.frame(var = df_2$var_2, row.names = paste(poly.names.list_2))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the dataframe
    SpDf_2 <- sp::SpatialPolygonsDataFrame(SpP_2, attr_2)

    # Convert the SpatialPolygonsDataFrame to an sf object and set the coordinate reference system
    SpDfSf_2 <- sf::st_as_sf(SpDf_2)
    sf::st_crs(SpDfSf_2) = '+proj=longlat +ellps=sphere'
    # If plot is FALSE, return the spatial polygons data frame
    if(plot == FALSE){
      return(SpDfSf)
    }

    # If plot is TRUE, proceed with plotting
    if(plot == TRUE){
      # If calculations are not to be performed, use the provided polygons
      if(calcs == FALSE){
        SpDfSf <- polygons
      }

      ## Outline of map using a framing line
      l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180),
                  c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
      L1 <- sp::Polygon(l1)
      Ls1 <- sp::Polygons(list(L1), ID="a")
      SLs1 <-  sp::SpatialPolygons(list(Ls1))

      df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
      names(df1)[1] <- "var"
      SLs1df = sp::SpatialPolygonsDataFrame(SLs1, data = df1)
      SLs1dfSf <- sf::st_as_sf(SLs1df)
      sf::st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

      # Define color scale parameters for the first variable (ocean data)
      min.value_1 <- min.value
      max.value_1 <- max.value
      intervals_1 <- intervals

      # Define color scale parameters for the second variable (land data)
      min.value_2 <- 0
      max.value_2 <- 3200
      intervals_2 <- 400

  # Set colors based on dark mode
  if (darkmode) {
    bg.color <- "black"
    fg.color <- "white"
  }

      if(is.null(col.labels)){
        col.labels <- seq(min.value_1, max.value_1, intervals_1)

      }
  # If the map is not for an app, create the plot with default settings
  if (for.app == FALSE) {
    map <- ggplot2::ggplot() +
      ggspatial::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = unit.factor * var), color = NA, linewidth = 10, linetype = 0) + # Plot ocean data
      ggspatial::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry), fill = NA, color = fg.color, linewidth = 0.9) + # Add map outline
      ggplot2::scale_fill_stepsn(colours = palette_name_ocean,
                        guide = ggspatial::guide_colorbar(title.position = "top",
                                               barwidth = 12,
                                               barheight = 1,
                                               raster = FALSE,
                                               frame.colour = fg.color,
                                               frame.linewidth = 2 / .pt,
                                               frame.linetype = 1,
                                               ticks = TRUE,
                                               ticks.colour = fg.color,
                                               ticks.linewidth = 2 / .pt),
                        breaks = seq(min.value_1, max.value_1, intervals_1),
                        limits = c(min.value_1, max.value_1), labels = col.labels) +
      theme(legend.position = "bottom",
            plot.background = ggspatial::element_rect(fill = bg.color),
            text = ggspatial::element_text(colour = fg.color)) +
      ggplot2::labs(fill = scale.label) +
      ggnewscale::new_scale_fill() +
      ggspatial::geom_sf(data = SpDfSf_2 %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = var), color = NA, linewidth = 10, linetype = 0) + # Plot land data
      ggplot2::scale_fill_stepsn(colours = palette_name_land,
                        guide = ggspatial::guide_colorbar(title.position = "top",
                                               barwidth = 12,
                                               barheight = 1,
                                               raster = FALSE,
                                               frame.colour = fg.color,
                                               frame.linewidth = 2 / .pt,
                                               frame.linetype = 1,
                                               ticks = TRUE,
                                               ticks.colour = fg.color,
                                               ticks.linewidth = 2 / .pt),
                        breaks = seq(min.value_2, max.value_2, intervals_2),
                        limits = c(min.value_2, max.value_2)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
            plot.background = ggspatial::element_rect(fill = bg.color),
            text = ggspatial::element_text(colour = fg.color)) +
      ggplot2::labs(fill = 'Topography (m)')
  }

  # If the map is for an app, create the plot with app-specific settings
  if (for.app == TRUE) {
    map <- ggplot2::ggplot() +
      ggspatial::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = var), color = NA, linewidth = 10, linetype = 0) + # Plot ocean data
      ggspatial::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry), fill = NA, color = fg.color, linewidth = 0.9) + # Add map outline
      ggplot2::scale_fill_stepsn(colours = palette_name_ocean,
                        guide = ggspatial::guide_colorbar(title.position = "top",
                                               barwidth = 12,
                                               barheight = 1,
                                               raster = FALSE,
                                               frame.colour = fg.color,
                                               frame.linewidth = 2 / .pt,
                                               frame.linetype = 1,
                                               ticks = TRUE,
                                               ticks.colour = fg.color,
                                               ticks.linewidth = 2 / .pt),
                        breaks = seq(min.value_1, max.value_1, intervals_1),
                        limits = c(min.value_1, max.value_1), labels = col.labels) +
      ggplot2::theme(legend.position = "bottom",
            plot.background = ggspatial::element_rect(fill = bg.color),
            text = ggspatial::element_text(colour = fg.color)) +
      ggplot2::labs(fill = 'Sea Surface Temperature (\u00B0C)') +
      ggnewscale::new_scale_fill() +
      ggspatial::geom_sf(data = SpDfSf_2 %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = var), color = NA, linewidth = 10, linetype = 0) + # Plot land data
      ggplot2::scale_fill_stepsn(colours = palette_name_land,
                        guide = ggspatial::guide_colorbar(title.position = "top",
                                               barwidth = 12,
                                               barheight = 1,
                                               raster = FALSE,
                                               frame.colour = fg.color,
                                               frame.linewidth = 2 / .pt,
                                               frame.linetype = 1,
                                               ticks = TRUE,
                                               ticks.colour = fg.color,
                                               ticks.linewidth = 2 / .pt),
                        breaks = seq(min.value_2, max.value_2, intervals_2),
                        limits = c(min.value_2, max.value_2)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
            line = ggspatial::element_line(colour = fg.color),
            plot.background = ggspatial::element_rect(fill = bg.color),
            text = ggspatial::element_text(colour = fg.color)) +
      ggplot2::labs(fill = 'Topography (m)')
  }

  # Return the generated map
  map
}}
