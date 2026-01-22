#' Generate Maps from HADCM3L Model Output with Points Matching
#'
#' This function generates maps from imported .nc files, such as those from Valdes et al. 2021. It can handle both 2D and 3D netCDF files and provides options for various customizations including projections, color palettes, and more.
#' Additionally, it matches and plots specific points from a provided data frame.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file. (e.g. "insitu_T_ym_dpth")
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param depth.level (numeric) Depth layer to visualize (default = 1).
#' @param dims (numeric) Dimensionality of the data (default NULL).
#' @param year (numeric or character) Time step to visualize (default "default").
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#' @param max_dist Maximum allowed distance for matching (km, default 1000 km).
#' @param na.method How to handle NA climate values: "nearest" (default), "same.lat", "keep".
#' @param unit.factor (numeric) Scaling factor for variable (default NULL).
#' @param min.value,max.value (numeric) Color scale limits (default NULL).
#' @param intervals (numeric) Color scale intervals (default NULL).
#' @param continents.outlined (logical) Whether to outline continents (default TRUE).
#' @param scale.label (character) Label for color scale (default NULL).
#' @param line.thickness (numeric) Thickness of continent outlines (default 1).
#' @param palette_name (character) Color palette for the plot (default pals::parula(1000)).
#' @param projection (character) Map projection (default 'ESRI:54012').
#' @param darkmode (logical) Enable dark mode (default FALSE).
#' @param custom.bg (character) Background color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param custom.fg (character) Foreground color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param col.labels (numeric) Labels for the color bar ticks.
#' @param show.NAs (logical) Whether to show NA values on the map (default FALSE).
#' @return A ggplot object representing the map with matched points.
#' @details
#' This function reads 2D or 3D data from HADCM3L model output NetCDF files and produces a map visualization.
#' Users can specify their own scaling and color settings.
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

HADCM3.points.map <- function(var,
       file,
       experiment,
       depth.level = 1,
       dims = NULL,
       year = "default",
       coord.dat, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
       lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
       lng.name = "p_lng", # name IF generated from rotated paleoverse coordinates...
       max_dist = 1000,
       na.method = "nearest",
       unit.factor = NULL,
       min.value = NULL,
       max.value = NULL,
       intervals = NULL,
       continents.outlined = TRUE,
       scale.label = NULL,
       model = "biogem",
       line.thickness = 1,
       palette_name = pals::parula(1000),
       projection = 'ESRI:54012',
       darkmode = FALSE,
       custom.bg = NULL,
       custom.fg = NULL,
       col.labels = NULL,
       show.NAs = TRUE){


  # Color bar labels
  if(is.null(col.labels)) col.labels <- seq(min.value, max.value, intervals)

  # Dark mode defaults
  if(darkmode){
    if(is.null(custom.bg)) custom.bg <- "black"
    if(is.null(custom.fg)) custom.fg <- "white"
  } else {
    if(is.null(custom.bg)) custom.bg <- "white"
    if(is.null(custom.fg)) custom.fg <- "black"
  }

  matched_points <- HADCM3.points.data(var = var,
                                       file = file,
                                       experiment = experiment,
                                       depth.level = depth.level,
                                       dims = dims,
                                       year = year,
                                       coord.dat = coord.dat,
                                       lat.name = lat.name,
                                       lng.name = lng.name,
                                       max_dist = max_dist,
                                       na.method = na.method
  )


  clim.map <- HADCM3.map(var = var,
                         file = file,
                         experiment = experiment,
                         depth.level = depth.level,
                         dims = dims,
                         year = year,
                         unit.factor = unit.factor,
                         min.value = min.value,
                         max.value = max.value,
                         intervals = intervals,
                         continents.outlined = continents.outlined,
                         scale.label = scale.label,
                         line.thickness = line.thickness,
                         palette_name = palette_name,
                         projection = projection,
                         darkmode = darkmode,
                         col.labels = col.labels)

  matched_points$lat <- matched_points$p_lat
  matched_points$lng <- matched_points$p_lng

  # Create spatial object with the chosen points from start of script
  points <- as.data.frame(cbind(matched_points$lng, matched_points$lat, matched_points$matched_climate*unit.factor))
  if(show.NAs == FALSE){
    points <- stats::na.omit(points)
  }
  points_sp <- sp::SpatialPointsDataFrame(coords = points[,1:2], data = as.data.frame(points[,3]))
  names(points_sp) <- "matched_climate"

  # code in colour scale for matched points
  palette_name_points <- palette_name
  min.value_1 <- min.value
  max.value_1 <- max.value
  intervals_1 <- intervals
  # make plottable object
  points_spsf <- sf::st_as_sf(points_sp)
  sf::st_crs(points_spsf) <- 4326

  # Plot map with points
  map.points <- clim.map +
    ggplot2::geom_sf(data = sf::st_transform(points_spsf, projection), ggplot2::aes(geometry = geometry, fill = .data$matched_climate), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

  return(map.points)
}
