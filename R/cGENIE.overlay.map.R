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
#' @param depth.level The depth level to extract from the .nc file. Defaults to 1.
#' @param dims The number of dimensions for the data. Defaults to 3.
#' @param year The year to extract from the .nc file. Defaults to "default".
#' @param unit.factor The factor to multiply the variable values by. Defaults to 1.
#' @param continents.outlined Logical indicating whether to outline continents. Defaults to TRUE.
#' @param line.thickness The thickness of the outline lines. Defaults to 1.
#' @param palette_name The color palette to use for the map. Defaults to pals::parula(1000).
#' @param projection The map projection to use. Defaults to 'ESRI:54012'.
#' @param darkmode Logical indicating whether to use dark mode. Defaults to FALSE.
#' @param bg_color The background color of the map. Defaults to "white" if darkmode is FALSE, "black" if darkmode is TRUE.
#' @param fg_color The foreground color of the map. Defaults to "black" if darkmode is FALSE, "white" if darkmode is TRUE.
#' @return A ggplot object representing the overlay map.
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @importFrom sf st_as_sf
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom ggspatial annotation_map_tile
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn
#' @export
#' 
cGENIE.overlay.map <- function(var, experiment,
  depth.level = 1,
  dims = 3,
  year = "default",
  unit.factor = 1,
  min.value,
  max.value,
  intervals,
  continents.outlined = TRUE,
  scale.label,
  line.thickness = 1,
  model = "biogem",
  palette_name = pals::parula(1000),
  projection = 'ESRI:54012',
  darkmode = FALSE,
  bg_color = ifelse(darkmode, "black", "white"),
  fg_color = ifelse(darkmode, "white", "black")){
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
    if (model == "biogem") {
      prefix <- "/biogem/fields_biogem_"
    }

    # Open the netCDF file corresponding to the experiment and model type
    nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

    # Extract general variables
    lat <- RNetCDF::var.get.nc(nc, "lat") # units: degrees north
    lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")
    lon <- RNetCDF::var.get.nc(nc, "lon") # units: degrees east
    lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")
    depth <- RNetCDF::var.get.nc(nc, "zt") # units: metres
    depth.edges <- RNetCDF::var.get.nc(nc, "zt_edges") # units: metres
    time <- RNetCDF::var.get.nc(nc, "time") # units: year mid-point
    var.arr <- RNetCDF::var.get.nc(nc, var) # Extract named variable
    oxy.arr <- RNetCDF::var.get.nc(nc, "ocn_sur_temp") # Extract oxygen variable arbitrarily to get land mask

    # Determine the time step to use
    if (year == "default") {
      time.step <- length(time)  # Use the last time step if year is set to default
    } else {
      time.step <- year  # Use the specified year as the time step
    }

    # Adjust longitude values to be within the range 0 to 360 degrees
    if (mean(dplyr::between(lon, -180, 180)) < 1) {
      lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
      lon[lon <= -180] <- lon[lon <= -180] + 360
    }

    if (dims == 3) {
      # Generate a dataframe for a 2D slice from a 3D array
      df <- as.data.frame(cbind(
        rep(lon, times = length(lat), each = 1),
        rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
        rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
        rep(lat, times = 1, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
        rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
        reshape2::melt(var.arr[,, depth.level, time.step])$value
      ))
      names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    }

    if (dims == 2) {
      # Generate a dataframe for a 2D array
      df <- as.data.frame(cbind(
        rep(lon, times = length(lat), each = 1),
        rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
        rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
        rep(lat, times = 1, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
        rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
        reshape2::melt(var.arr[,, time.step])$value
      ))
      names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
    }

    # Create a dataframe for the oxygen variable to get the land mask
    df2 <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      reshape2::melt(oxy.arr[,, time.step])$value
    ))
    names(df2) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "oxy")

    # Eliminate cells outside of the reasonable range for the main variable dataframe
    df <- df %>%
      dplyr::filter(
        lon.max <= 180,
        lon.min >= -180,
        lat.max <= 90,
        lat.min >= -90
      )

    # Eliminate cells outside of the reasonable range AND only select NA cells for the oxygen dataframe
    df2 <- df2 %>%
      dplyr::filter(
        lon.max <= 180,
        lon.min >= -180,
        lat.max <= 90,
        lat.min >= -90,
        is.na(oxy)
      )

    # Update cells that bridge left and right side of map
    df$lon.range <- abs(df$lon.min - df$lon.max)
    df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
    df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

    df2$lon.range <- abs(df2$lon.min - df2$lon.max)
    df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180] <- -df2$lon.min[df2$lon.range > 180 & abs(df2$lon.min) == 180]
    df2$lon.max[df2$lon.range > 180 & abs(df2$lon.max) == 180] <- -df2$lon.max[df2$lon.range > 180 & abs[df2$lon.max) == 180]

    # Create a list to store polygons for the main variable dataframe
    poly.list <- list()
    poly.names.list <- list()
    for (poly in 1:(nrow(df))) {
      polygon.code <- sp::Polygon(cbind(
        c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
        c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])
      ))
      polygons.code <- sp::Polygons(list(polygon.code), paste0("p", poly))
      poly.list <- append(poly.list, polygons.code)
      poly.names.list <- append(poly.names.list, paste0("p", poly))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP <- sp::SpatialPolygons(poly.list)
    # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
    attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the attributes dataframe
    SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)

    # Convert the SpatialPolygonsDataFrame to an sf object
    SpDfSf <- sf::st_as_sf(SpDf)
    sf::st_crs(SpDfSf) <- '+proj=longlat +ellps=sphere'

    # Create polygons for NA cells (land mask)
    poly2.list <- list()
    poly2.names.list <- list()
    for(poly2 in 1:(nrow(df2))){
      # Create a polygon for each cell in the dataframe
      polygon.code2 <- sp::Polygon(cbind(
      c(df2$lon.min[poly2], df2$lon.max[poly2], df2$lon.max[poly2], df2$lon.min[poly2]),
      c(df2$lat.min[poly2], df2$lat.min[poly2], df2$lat.max[poly2], df2$lat.max[poly2])
      ))
      assign(paste0("polygon_", poly2), polygon.code2)

      # Create a Polygons object for each polygon
      polygons.code2 <- sp::Polygons(list(polygon.code2), paste0("p", poly2))
      assign(paste0("polygons_", poly2), polygons.code2)

      # Append the Polygons object to the list
      poly2.list <- append(poly2.list, polygons.code2)
      poly2.names.list <- append(poly2.names.list, paste0("p", poly2))
    }

    # Create a SpatialPolygons object from the list of Polygons objects
    SpP2 <- sp::SpatialPolygons(poly2.list)

    # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
    attr2 <- data.frame(var = df2$oxy, row.names = paste(poly2.names.list))

    # Create a SpatialPolygonsDataFrame from the SpatialPolygons object and the attributes dataframe
    Spdf2 <- sp::SpatialPolygonsDataFrame(SpP2, attr2)

    # Convert the SpatialPolygonsDataFrame to an sf object
    Spdf2Sf <- sf::st_as_sf(Spdf2)
    sf::st_crs(Spdf2Sf) <- '+proj=longlat +ellps=sphere'

    # Create an outline of the map using a framing line
    l1 <- cbind(
      c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180),
      c(-90, -90, seq(-90, 90, 0.1), 90, 90, seq(90, -90, -0.1), -90)
    )
    L1 <- sp::Polygon(l1)
    Ls1 <- sp::Polygons(list(L1), ID = "a")
    SLs1 <- sp::SpatialPolygons(list(Ls1))

    # Create a dataframe for the attributes of the SpatialPolygonsDataFrame
    df1 <- data.frame(rep(2, 1), row.names = rep("a", 1))
    names(df1)[1] <- "var"
    SLs1df <- sp::SpatialPolygonsDataFrame(SLs1, data = df1)

    # Convert the SpatialPolygonsDataFrame to an sf object
    SLs1dfSf <- sf::st_as_sf(SLs1df)
    sf::st_crs(SLs1dfSf) <- '+proj=longlat +ellps=sphere'

    continent_polygons <- dplyr::filter(df2, is.na(oxy))

    poly.list.continents <- list()
    for (poly in 1:(nrow(continent_polygons))) {
      polygon.code <- sp::Polygon(cbind(
        c(continent_polygons$lon.min[poly], continent_polygons$lon.max[poly], continent_polygons$lon.max[poly], continent_polygons$lon.min[poly]),
        c(continent_polygons$lat.min[poly], continent_polygons$lat.min[poly], continent_polygons$lat.max[poly], continent_polygons$lat.max[poly])))
      polygons.code <- sp::Polygons(list(polygon.code), paste0("p", poly))
      poly.list.continents <- append(poly.list.continents, polygons.code)
    }

    SpP.continents <- sp::SpatialPolygons(poly.list.continents)
    attr.continents <- data.frame(row.names = sapply(poly.list.continents, function(x) x@ID))
    SpDf.continents <- sp::SpatialPolygonsDataFrame(SpP.continents, attr.continents)
    SpDfSf.continents <- sf::st_as_sf(SpDf.continents)
    sf::st_crs(SpDfSf.continents) = '+proj=longlat +ellps=sphere'

    continents <- sf::st_union(SpDfSf.continents)

    map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf::st_transform(SpDfSf, projection), ggplot2::aes(geometry = geometry, fill=var*unit.factor), color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    ggplot2::geom_sf(data = sf::st_transform(SLs1dfSf, projection), ggplot2::aes(geometry = geometry), fill=NA, color = fg_color, linewidth=0.9) +
    ggplot2::geom_sf(data = sf::st_transform(Spdf2Sf, projection), ggplot2::aes(geometry = geometry), fill = "grey80", alpha = 1, color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
      ggplot2::geom_sf(data = sf::st_transform(sf::st_as_sf(continents), projection), fill = "grey80", color = "grey20", linewidth = line.thickness) +
      ggplot2::scale_fill_stepsn(colours = palette_name,
        guide = ggplot2::guide_colorbar(title.position = "top",
          barwidth = 12,
          barheight = 1,
          raster = FALSE,
          frame.linewidth = 2/.pt,
          frame.linetype = 1,
          ticks = TRUE,
          ticks.linewidth = 2/.pt,
          frame.colour = fg_color,
          ticks.colour = fg_color),
        breaks = seq(min.value, max.value, intervals),
        limits=c(min.value, max.value)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
      plot.background = ggplot2::element_rect(fill = bg_color),
      panel.background = ggplot2::element_rect(fill = bg_color),
      legend.background = ggplot2::element_rect(fill = bg_color),
      legend.text = ggplot2::element_text(color = fg_color),
      legend.title = ggplot2::element_text(color = fg_color),
      axis.text = ggplot2::element_text(color = fg_color),
      axis.title = ggplot2::element_text(color = fg_color),
      plot.title = ggplot2::element_text(color = fg_color)) +
    ggplot2::labs(fill = scale.label)

  map
}
