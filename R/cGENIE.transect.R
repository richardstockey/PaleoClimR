#' Create Transect Plots from cGENIE Model Output
#'
#' This function creates transect plots from cGENIE model output .nc files.
#'
#' @param experiment Directory containing the experiment's netCDF files.
#' @param var The variable name to extract from the .nc file.
#' @param slice The slice index to extract from the data. Defaults to 1.
#' @param dims Number of dimensions in the netCDF file. Defaults to 3.
#' @param lat.or.lon Whether to create a transect along latitude or longitude. Defaults to "lat".
#' @param year Year to extract data for (default is "default", meaning the last time point).
#' @param unit.factor Factor to convert units. Defaults to 1.
#' @param min.value Minimum value for the color scale. Defaults to 0.
#' @param max.value Maximum value for the color scale. Defaults to 40.
#' @param intervals Number of intervals for the color scale. Defaults to 5.
#' @param continents.outlined Logical indicating whether to outline continents. Defaults to NULL.
#' @param scale.label Label for the color scale. Defaults to NULL.
#' @param model The model type; defaults to "biogem".
#' @param palette_name Color palette to use for the plot. Defaults to "pals::parula(1000)".
#' @param projection Projection to use for the plot. Defaults to "ESRI:54012".
#' @return A ggplot object representing the transect plot.
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn
#' @importFrom pals parula
#' @export

cGENIE.transect <- function(experiment, var,
        slice = 1,
        dims = 3,
        lat.or.lon = "lat",
        year = "default",
        unit.factor = 1,
        min.value = 0,
        max.value = 40,
        intervals = 5,
        continents.outlined,
        scale.label = NULL,
        model = "biogem",
        palette_name = pals::parula(1000),
        projection = 'ESRI:54012'){

  # Load necessary libraries
  library(RNetCDF)  # For handling NetCDF files
  library(dplyr)    # For data manipulation
  library(reshape2) # For melting arrays into data frames
  library(ggplot2)  # For creating plots
  library(pals)     # For color palettes

  # Set file prefix based on model type
  if(model == "biogem"){
    prefix <- "/biogem/fields_biogem_"
  }

  # Open the NetCDF file
  nc <- open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  # Extract general variables from the NetCDF file
  lat <- var.get.nc(nc, "lat")  # Latitude values
  lat.edges <- var.get.nc(nc, "lat_edges")  # Latitude edge values
  lon <- var.get.nc(nc, "lon")  # Longitude values
  lon.edges <- var.get.nc(nc, "lon_edges")  # Longitude edge values
  depth <- var.get.nc(nc, "zt")  # Depth values
  depth.edges <- var.get.nc(nc, "zt_edges")  # Depth edge values
  time <- var.get.nc(nc, "time")  # Time values
  var.arr <- var.get.nc(nc, var)  # Extract the specified variable

  # Determine the time step to use
  if(year == "default"){
  time.step <- length(time)  # Use the last time step if year is "default"
  } else {
  time.step <- year  # Use the specified year as the time step
  }

  # Adjust longitude values if necessary
  if(mean(between(lon, -180, 180)) < 1){
  lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
  lon[lon <= -180] <- lon[lon <= -180] + 360
  }

  # Generate dataframe for 3D data along latitude
  if(dims == 3 && lat.or.lon == "lat"){
  df <- as.data.frame(cbind(
    rep(lat, times = length(depth), each = 1),
    rep(lat.edges[1:(length(lat.edges)-1)], times = length(depth), each = 1),
    rep(lat.edges[2:(length(lat.edges))], times = length(depth), each = 1),
    rep(depth, times = 1, each = length(lat)),
    rep(depth.edges[1:(length(depth.edges)-1)], times = 1, each = length(lat)),
    rep(depth.edges[2:(length(depth.edges))], times = 1, each = length(lat)),
    as.data.frame(melt(var.arr[slice,,, time.step]))$value
  ))
  names(df) <- c("lat.mid", "lat.min", "lat.max", "depth.mid", "depth.min", "depth.max", "var")
  }

  # Generate dataframe for 3D data along longitude
  if(dims == 3 && lat.or.lon == "lon"){
  df <- as.data.frame(cbind(
    rep(lon, times = length(depth), each = 1),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(depth), each = 1),
    rep(lon.edges[2:(length(lon.edges))], times = length(depth), each = 1),
    rep(depth, times = 1, each = length(lon)),
    rep(depth.edges[1:(length(depth.edges)-1)], times = 1, each = length(lon)),
    rep(depth.edges[2:(length(depth.edges))], times = 1, each = length(lon)),
    as.data.frame(melt(var.arr[slice,,, time.step]))$value
  ))
  names(df) <- c("lon.mid", "lon.min", "lon.max", "depth.mid", "depth.min", "depth.max", "var")
  }

  # Adjust longitude values for cells that bridge the map edges
  if(lat.or.lon == "lon"){
  df$lon.range <- abs(df$lon.min - df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]
  }

    # Create the plot for latitude transect
  if(lat.or.lon == "lat"){
  map <- ggplot(data = df, aes(x = lat.mid, y = depth.mid, xmin = lat.min, xmax = lat.max, ymin = depth.min, ymax = depth.max, fill = var * unit.factor)) +
    geom_rect(color = NA, linewidth = 10, linetype = 0) +
    scale_fill_stepsn(colours = palette_name,
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
            limits = c(min.value, max.value)) +
    theme_bw() +
    scale_y_reverse() +
    theme(legend.position = "bottom",
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18)) +
    coord_cartesian(expand = FALSE) +
    ylab("Depth (m)") +
    xlab("Latitude (\u00B0)") +
    labs(fill = scale.label, title = paste0(lon[slice], "\u00B0 Longitude"))
  }

  # Create the plot for longitude transect
  if(lat.or.lon == "lon"){
  map <- ggplot(data = df, aes(x = lon.mid, y = depth.mid, xmin = lon.min, xmax = lon.max, ymin = depth.min, ymax = depth.max, fill = var * unit.factor)) +
    geom_rect(color = NA, linewidth = 10, linetype = 0) +
    scale_fill_stepsn(colours = palette_name,
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
            limits = c(min.value, max.value)) +
    theme_bw() +
    scale_y_reverse() +
    theme(legend.position = "bottom",
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18)) +
    coord_cartesian(expand = FALSE) +
    ylab("Depth (m)") +
    xlab("Longitude (\u00B0)") +
    labs(fill = scale.label, title = paste0(lat[slice], "\u00B0 Latitude"))
  }

  # Return the plot
  map
}