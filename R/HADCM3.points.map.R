#' HADCM3.map with Points Matching
#'
#' This function generates maps from imported .nc files, such as those from Valdes et al. 2021. It can handle both 2D and 3D netCDF files and provides options for various customizations including projections, color palettes, and more.
#' Additionally, it matches and plots specific points from a provided data frame.
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
#' @param palette_name Function. The color palette function to be used for the map. Default is `pals::parula(1000)`.
#' @param na.colour Character. The color to be used for NA values. Default is "grey80".

#'
#' @return Returns a ggplot object.
#' @import RNetCDF dplyr sf sp ggspatial reshape2 ggplot2 pals viridis
#' @export
HADCM3.points.map <- function(var,
       file,
       experiment,
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
       palette_name = pals::parula(1000),
       na.colour = "grey80",
       coord.dat = NULL, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
       lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
       lng.name = "p_lng" # name IF generated from rotated paleoverse coordinates...
){
  matched_points <- HADCM3.point.matching(var = var,
                                                      file = file,
                                                      experiment = experiment,
                                                      depth.level = depth.level,
                                                      dims = dims,
                                                      coord.dat = coord.dat,
                                                      lat.name = lat.name,
                                                      lng.name = lng.name
  )

  # Open the netCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, file, ".nc"))

  # Extract general variables
  # NOTE - these may not precisely represent the HADCM3 grid
  # fudged slightly for ease of plotting from the xxx and xxx_1 variables.
  # worth checking with HADCM3 users to be doubly sure
  # this should be kept separate from variable matching scripts with point data such as PBDB and therefore be functionally fine

  # Extract latitude values and calculate edges
  lat <- RNetCDF::var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

  # Extract longitude values and calculate edges
  lon <- RNetCDF::var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

  # If the netCDF file has 3 dimensions, extract depth values and calculate edges
  if(dims == 3){
    depth <- RNetCDF::var.get.nc(nc, "depth_1") # units: metres
    depth.edges <- c(0, RNetCDF::var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep
  }

  # If the netCDF file includes a time dimension, extract time values
  if(time.present == TRUE){
    time <- RNetCDF::var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
    # note that not all of these general variables will be available for fields_biogem_2d (address later)
  }

  # Extract the specified variable from the netCDF file
  var.arr <- RNetCDF::var.get.nc(nc, var)

  # NOTE - this is what i have done with cGENIE models.
  # Is this the best way to deal with here,
  # or just another way of translating to a nice grid?
  # maybe for plotting either is kind of fine.
  # but definitely would need to be fixed for point data matching.
  # deal with weird lon coordinates if present
  # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(dplyr::between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # Amend HADCM3 grid to project on 0 degrees
  if(mean(dplyr::between(lon, -180, 180)) < 1){
    lon.edges[lon.edges >180] <- lon.edges[lon.edges >180] - 360
    lon[lon >180] <- lon[lon >180] -360
  }

  # If the netCDF file has 3 dimensions, generate a dataframe of 2D slices from the 3D array
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
    # Generate dataframe of 2D slice from 2D array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(reshape2::melt(var.arr))$value))

    # Assign column names to the dataframe
    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )

    # Special handling for specific file and variable
    if(file == ".qrparm.orog" & var == "ht"){
      df$var <- as.factor(df$var)
      df <- dplyr::filter(df, var != "0")
      df$var <- as.numeric(paste(df$var))
    }
  }

  # Eliminate cells outside of reasonable range
  df <- df %>%
    dplyr::filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90,
           lat.max >= -90, # Adding this to handle flipped counting in specific files
           lat.min <= 90
    )

  # Eliminate cells that bridge the left and right side of the map (i.e., extreme -180ish and 180ish longitude)
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df <- df %>%
    dplyr::filter(lon.range < 180) # This condition works for all model grids

  # Initialize lists to store polygons and their names
  poly.list <- list()
  poly.names.list <- list()

  # Loop through each row in the dataframe to create polygons
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

  # Create SpatialPolygons object
  SpP <- sp::SpatialPolygons(poly.list)

  # Create a dataframe for the attributes
  attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

  # Create SpatialPolygonsDataFrame object
  SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)

  # Convert to sf object and set CRS
  SpDfSf <- sf::st_as_sf(SpDf)
  sf::st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'


    ## Outline of map using a framing line
    l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
    L1 <- sp::Polygon(l1)
    Ls1 <- sp::Polygons(list(L1), ID="a")
    SLs1 <-  sp::SpatialPolygons(list(Ls1))

    df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
    names(df1)[1] <- "var"
    SLs1df = sp::SpatialPolygonsDataFrame(SLs1, data = df1)
    SLs1dfSf <- sf::st_as_sf(SLs1df)
    sf::st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'

    # Create spatial object with the chosen points from start of script
    points <- as.data.frame(cbind(matched_points[[lng.name]], matched_points[[lat.name]], matched_points$matched_climate * unit.factor))
    points <- stats::na.omit(points)
    points_sp <- sp::SpatialPointsDataFrame(coords = points[,1:2], data = as.data.frame(points[,3]))
    names(points_sp) <- "matched_climate"

    # code in colour scale for matched points
    palette_name_points <- palette_name
    min.value_1 <- min.value
    max.value_1 <- max.value
    intervals_1 <- intervals

    # make plottable object
    points_spsf <- sf::st_as_sf(points_sp)
    sf::st_crs(points_spsf) = '+proj=longlat +ellps=sphere'

    # Create the ggplot object for the map
    map <- ggplot2::ggplot() +
      # Add the main spatial data layer, transforming the projection and setting the fill aesthetic
      ggplot2::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = var * unit.factor), color = NA, linewidth = 10, linetype = 0) +
      # Add the outline of the map using the framing line, transforming the projection and setting the color
      ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), color = ifelse(darkmode, foreground.colour, "grey5"), linewidth = 0.9, fill = NA) +
      # Define the color scale for the fill aesthetic using the specified palette
      ggplot2::scale_fill_stepsn(colours = palette_name,
              guide = ggplot2::guide_colorbar(title.position = "top",  # Position the title of the color bar at the top
                           barwidth = 12,           # Set the width of the color bar
                           barheight = 1,           # Set the height of the color bar
                           raster = FALSE,          # Disable rasterization of the color bar
                           frame.colour = ifelse(darkmode, foreground.colour, "grey6"),  # Set the frame color of the color bar
                           frame.linewidth = 2 / ggplot2::.pt,  # Set the frame linewidth of the color bar
                           frame.linetype = 1,      # Set the frame linetype of the color bar
                           ticks = TRUE,            # Enable ticks on the color bar
                           ticks.colour = ifelse(darkmode, foreground.colour, "grey6"),  # Set the color of the ticks
                           ticks.linewidth = 2 / ggplot2::.pt),  # Set the linewidth of the ticks
              breaks = seq(min.value, max.value, intervals),  # Define the breaks for the color scale
              limits = c(min.value, max.value),  # Set the limits for the color scale
              na.value = na.colour  # Set the color for NA values
      ) +
      # Apply a minimal theme to the plot
      ggplot2::theme_minimal() +
      # Position the legend at the bottom of the plot
      ggplot2::theme(legend.position = "bottom",
        plot.background = ggplot2::element_rect(fill = ifelse(darkmode, background.colour, "white"), color = NA),
        panel.background = ggplot2::element_rect(fill = ifelse(darkmode, background.colour, "white"), color = NA),
        text = ggplot2::element_text(color = ifelse(darkmode, foreground.colour, "black")),
        axis.text = ggplot2::element_text(color = ifelse(darkmode, foreground.colour, "black")),
        legend.text = ggplot2::element_text(color = ifelse(darkmode, foreground.colour, "black")),
        legend.title = ggplot2::element_text(color = ifelse(darkmode, foreground.colour, "black"))
      ) +
      # Set the label for the fill aesthetic
      ggplot2::labs(fill = scale.label)

    map.points <- map +
    ggplot2::geom_sf(data = points_spsf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill = matched_climate), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

    # Return the ggplot object
    return(map.points)
    }
