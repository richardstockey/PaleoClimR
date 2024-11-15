#' HADCM3.map.pretty
#'
#' This function generates visually appealing maps from imported .nc files, such as those from Valdes et al. 2021. It aims to include key information to visualize as much of the earth system as possible at one time.
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
#'
#' @return If plot is FALSE, returns a spatial polygons data frame. If plot is TRUE, returns a ggplot object.
#' @import RNetCDF dplyr sf sp ggspatial reshape2 ggplot2 ggnewscale
#' @export
#'

HADCM3.map.pretty <- function(experiment,
                        depth.level = 1,
                        dims = 3,
                       land.opt = "default",
                       min.value,
                       max.value,
                       intervals,
                       continents.outlined,
                       scale.label,
                       unit.factor = 1,
                       time.present = FALSE,
                       scale = "viridis",
                       projection = 'ESRI:54012',
                       calcs = TRUE,
                       plot = TRUE,
                       for.app = FALSE,
                       polygons){

  # Load necessary libraries for the function
  library(RNetCDF)    # For reading and manipulating netCDF files
  library(dplyr)      # For data manipulation
  library(sf)         # For handling spatial data
  library(sp)         # For spatial data classes and methods
  library(ggspatial)  # For spatial data visualization with ggplot2
  library(reshape2)   # For reshaping data
  library(ggplot2)    # For creating plots
  library(ggnewscale) # For adding multiple color scales to ggplot2

  # Define the color palette for ocean data visualization
  palette_name_ocean <- pals::parula(1000)  # Using the 'parula' color palette from the 'pals' package with 1000 color steps
  #palette_name_ocean <- viridis::magma(1000)  # Alternative: Using the 'magma' color palette from the 'viridis' package with 1000 color steps
  #palette_name_ocean <- viridis::plasma(1000)  # Alternative: Using the 'plasma' color palette from the 'viridis' package with 1000 color steps

  # Define the color palette for land data visualization
  #palette_name_land <- paletteer::paletteer_c("grDevices::Terrain 2", 30)  # Alternative: Using the 'Terrain 2' color palette from the 'grDevices' package with 30 color steps
  palette_name_land <- paletteer::paletteer_c("grDevices::Light Grays", 30)  # Using the 'Light Grays' color palette from the 'grDevices' package with 30 color steps

  # Define the file and variable to be used for the netCDF data
  file <- "o.pgclann"  # Name of the netCDF file (without extension)
  var <- "insitu_T_ym_dpth"  # Name of the variable to extract from the netCDF file

  # Conditional statement to open the netCDF file if calculations are to be performed
  if(calcs == TRUE){
    # Construct the full path to the netCDF file by concatenating the experiment path and file name
    nc <- open.nc(paste0(experiment, file, ".nc"))
  }

  # Extract general variables
  # NOTE - these may not precisely represent the HADCM3 grid
  # fudged slightly for ease of plotting from the xxx and xxx_1 variables.
  # worth checking with HADCM3 users to be doubly sure
  # this should be kept separate from variable matching scripts with point data such as PBDB and therefore be functionally fine
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  depth <- var.get.nc(nc, "depth_1") # units: metres
  depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep
  if(time.present == TRUE){
  time <- var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
  # note that not all of these general variables will be available for fields_biogem_2d (address later)
  }

  # Extract named variable from the netCDF file
  var.arr <- var.get.nc(nc, var)

  # Now we're going to load a second netCDF file with orography values
  # Define the file name and variable name for the second netCDF file
  file_2 <- ".qrparm.orog"  # Name of the second netCDF file (without extension)
  var_2 <- "ht"  # Name of the variable to extract from the second netCDF file

  # Open the second netCDF file by constructing the full path using the experiment path and file name
  nc_2 <- open.nc(paste0(experiment, file_2, ".nc"))

  # Extract the second named variable from the second netCDF file
  var.arr_2 <- var.get.nc(nc_2, var_2)

  # # NOTE - this is what i have done with cGENIE models.
  # # Is this the best way to deal with here,
  # # or just another way of translating to a nice grid?
  # # maybe for plotting either is kind of fine.
  # # but definitely would need to be fixed for point data matching.
  # # deal with weird lon coordinates if present
  # # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # Amend HADCM3 grid to project on 0 degrees
  # Check if longitude values are within the range -180 to 180
  if(mean(between(lon, -180, 180)) < 1){
    # Adjust longitude edges and values greater than 180 by subtracting 360 to bring them within the range
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges > 180] - 360
    lon[lon > 180] <- lon[lon > 180] - 360
  }

  # Generate dataframe of 2D slice from 3D array
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),  # Repeat longitude values for each latitude
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),  # Repeat longitude edges (min) for each latitude
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),  # Repeat longitude edges (max) for each latitude
    rep(lat, times = 1, each = length(lon)),  # Repeat latitude values for each longitude
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),  # Repeat latitude edges (min) for each longitude
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),  # Repeat latitude edges (max) for each longitude
    as.data.frame(melt(var.arr[,, depth.level]))$value,  # Melt the 3D array to get variable values at the specified depth level
    as.data.frame(melt(var.arr_2))$value  # Melt the second variable array to get its values
  ))

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
  # }
  # if(dims == 2){
  #   # generate dataframe of 2d genie slice from 3d genie array
  #   df <- as.data.frame(cbind(
  #     rep(lon, times = length(lat), each = 1),
  #     rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
  #     rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
  #     rep(lat, times = 1, each = length(lon)),
  #     rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
  #     rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
  #     #as.data.frame(melt(var.arr[,, time.step]))$value))
  #     as.data.frame(melt(var.arr[,, time.step]))$value))
  #
  #   names(df) <- c("lon.mid",
  #                  "lon.min",
  #                  "lon.max",
  #                  "lat.mid",
  #                  "lat.min",
  #                  "lat.max",
  #                  "var"
  #   )
  # }

    # Eliminate cells outside of the reasonable range for longitude and latitude
    df <- df %>%
      filter(lon.max <= 180,  # Ensure maximum longitude is within 180 degrees
         lon.min >= -180, # Ensure minimum longitude is within -180 degrees
         lat.max <= 90,   # Ensure maximum latitude is within 90 degrees
         lat.min >= -90   # Ensure minimum latitude is within -90 degrees
         )

    # Eliminate cells that bridge the left and right sides of the map (i.e., extreme -180ish and 180ish longitude)
    df$lon.range <- abs(df$lon.min - df$lon.max)  # Calculate the range of longitude for each cell
    df <- df %>%
      filter(lon.range < 180 # Filter out cells with a longitude range greater than or equal to 180 degrees
      )

    # Currently have some issues with 0s in the orography raster.
    # For now, just getting rid of them - later return to 0s vs 0.0s
    # Assuming all layers have the same grid
    df_2 <- df  # Create a copy of the dataframe for the second variable
    df_2$var_2 <- as.factor(df_2$var_2)  # Convert the second variable to a factor
    df_2 <- filter(df_2, var_2 != "0")  # Filter out cells where the second variable is 0
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
      polygon.code <- Polygon(cbind(
        c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
        c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
      assign(paste0("Polygon_", poly), polygon.code)

      # Create a Polygons object from the polygon
      polygons.code <- Polygons(list(polygon.code), paste0("p",poly))
      assign(paste0("Polygons_", poly), polygons.code)

      # Append the Polygons object and its name to the respective lists
      poly.list <- append(poly.list, polygons.code)
      poly.names.list <- append(poly.names.list, paste0("p",poly))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP <- SpatialPolygons(poly.list)

    # Create a dataframe with the variable values and row names
    attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the dataframe
    SpDf <- SpatialPolygonsDataFrame(SpP, attr)

    # Convert the SpatialPolygonsDataFrame to an sf object and set the coordinate reference system
    SpDfSf <- st_as_sf(SpDf)
    st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'

    # Initialize lists to store polygons and their names for the second variable
    poly.list_2 <- list()
    poly.names.list_2 <- list()

    # Loop through each row in the dataframe for the second variable to create polygons
    for(poly_2 in 1:(nrow(df_2))){
      # Create a polygon using the longitude and latitude edges
      polygon.code_2 <- Polygon(cbind(
        c(df_2$lon.min[poly_2], df_2$lon.max[poly_2], df_2$lon.max[poly_2], df_2$lon.min[poly_2]),
        c(df_2$lat.min[poly_2], df_2$lat.min[poly_2], df_2$lat.max[poly_2], df_2$lat.max[poly_2])))
      assign(paste0("Polygon_", poly_2), polygon.code_2)

      # Create a Polygons object from the polygon
      polygons.code_2 <- Polygons(list(polygon.code_2), paste0("p",poly_2))
      assign(paste0("Polygons_", poly_2), polygons.code_2)

      # Append the Polygons object and its name to the respective lists
      poly.list_2 <- append(poly.list_2, polygons.code_2)
      poly.names.list_2 <- append(poly.names.list_2, paste0("p",poly_2))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP_2 <- SpatialPolygons(poly.list_2)

    # Create a dataframe with the variable values and row names
    attr_2 <- data.frame(var = df_2$var_2, row.names = paste(poly.names.list_2))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the dataframe
    SpDf_2 <- SpatialPolygonsDataFrame(SpP_2, attr_2)

    # Convert the SpatialPolygonsDataFrame to an sf object and set the coordinate reference system
    SpDfSf_2 <- st_as_sf(SpDf_2)
    st_crs(SpDfSf_2) = '+proj=longlat +ellps=sphere'
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
      L1 <- Polygon(l1)
      Ls1 <- Polygons(list(L1), ID="a")
      SLs1 <-  SpatialPolygons(list(Ls1))

      df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
      names(df1)[1] <- "var"
      SLs1df = SpatialPolygonsDataFrame(SLs1, data = df1)
      SLs1dfSf <- st_as_sf(SLs1df)
      st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

      # Define color scale parameters for the first variable (ocean data)
      min.value_1 <- 0
      max.value_1 <- 40
      intervals_1 <- 5

      # Define color scale parameters for the second variable (land data)
      min.value_2 <- 0
      max.value_2 <- 3200
      intervals_2 <- 400

      # If the map is not for an app, create the plot with default settings
      if(for.app == FALSE){
        map <- ggplot() +
          geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # Plot ocean data
          geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), fill=NA, color = "grey5", linewidth=0.9) + # Add map outline
          scale_fill_stepsn(colours = palette_name_ocean,
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
                            breaks = seq(min.value_1, max.value_1, intervals_1),
                            limits=c(min.value_1, max.value_1)) +
          theme(legend.position="bottom") +
          labs(fill = 'Sea Surface Temperature (°C)') +
          new_scale_fill() +
          geom_sf(data = SpDfSf_2 %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # Plot land data
          scale_fill_stepsn(colours = palette_name_land,
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
                            breaks = seq(min.value_2, max.value_2, intervals_2),
                            limits=c(min.value_2, max.value_2)) +
          theme_minimal() +
          theme(legend.position="bottom") +
          labs(fill = 'Topography (m)')
      }

      # If the map is for an app, create the plot with app-specific settings
      if(for.app == TRUE){
        map <- ggplot() +
          geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # Plot ocean data
          geom_sf(data = SLs1dfSf %>% st_transform(projection), aes(geometry = geometry), fill=NA, color = "grey50", linewidth=0.9) + # Add map outline
          scale_fill_stepsn(colours = palette_name_ocean,
                            guide = guide_colorbar(title.position = "top",
                                                   barwidth = 12,
                                                   barheight = 1,
                                                   raster = FALSE,
                                                   frame.colour = "grey50",
                                                   frame.linewidth = 2/.pt,
                                                   frame.linetype = 1,
                                                   ticks = TRUE,
                                                   ticks.colour = "grey50",
                                                   ticks.linewidth = 2/.pt),
                            breaks = seq(min.value_1, max.value_1, intervals_1),
                            limits=c(min.value_1, max.value_1)) +
          theme(legend.position="bottom") +
          labs(fill = 'Sea Surface Temperature (°C)') +
          new_scale_fill() +
          geom_sf(data = SpDfSf_2 %>% st_transform(projection), aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # Plot land data
          scale_fill_stepsn(colours = palette_name_land,
                            guide = guide_colorbar(title.position = "top",
                                                   barwidth = 12,
                                                   barheight = 1,
                                                   raster = FALSE,
                                                   frame.colour = "grey50",
                                                   frame.linewidth = 2/.pt,
                                                   frame.linetype = 1,
                                                   ticks = TRUE,
                                                   ticks.colour = "grey50",
                                                   ticks.linewidth = 2/.pt),
                            breaks = seq(min.value_2, max.value_2, intervals_2),
                            limits=c(min.value_2, max.value_2)) +
          theme_minimal() +
          theme(legend.position="bottom",
                line = element_line(colour = "white"),
                plot.background = element_rect(fill = "black"),
                text = element_text(colour = "white")) +
          labs(fill = 'Topography (m)')
      }

      # Return the generated map
      map
    }
}
