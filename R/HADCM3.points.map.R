#' Generate Maps from HADCM3L Model Output with Points Matching
#'
#' This function generates maps from imported HADCM3L .nc files. It can handle
#' 2D and 3D NetCDF files and can match and plot points from a provided data
#' frame. Points can be coloured using the matched HADCM3 value or an independent
#' continuous or categorical variable from the point data.
#'
#' @param var A character string specifying the NetCDF variable (e.g. "insitu_T_ym_dpth").
#' @param file NetCDF file name without the .nc extension.
#' @param experiment Path to the HADCM3 experiment directory.
#' @param depth.level Numeric depth layer to visualize (default = 1).
#' @param dims Numeric dimensionality of the data (default NULL; auto-set by var).
#' @param year Numeric or character time step (default "default").
#' @param coord.dat Data frame containing the point coordinates and optional point-colouring data.
#' @param lat.name Name of the latitude column in `coord.dat` (default "p_lat").
#' @param lng.name Name of the longitude column in `coord.dat` (default "p_lng").
#' @param max_dist Maximum matching distance in km (default 1000).
#' @param na.method How to handle missing climate values: "nearest", "same.lat", or "keep".
#' @param unit.factor Numeric scaling factor for the mapped variable (default NULL).
#' @param min.value,max.value Numeric limits for the map colour scale (default NULL).
#' @param intervals Numeric interval for map colour breaks (default NULL).
#' @param continents.outlined Logical; outline continents (default TRUE).
#' @param scale.label Label for the map colour scale (default NULL).
#' @param line.thickness Thickness of continent outlines (default 1).
#' @param palette_name Colour palette for the map (default `pals::parula(1000)`).
#' @param projection Map projection (default "ESRI:54012").
#' @param darkmode Logical; use dark mode (default FALSE).
#' @param custom.bg,custom.fg Optional background and foreground colours.
#' @param col.labels Numeric labels for the map colour bar (default NULL).
#' @param show.NAs Logical; show points with missing values (default TRUE).
#' @param points.col Optional name of a column in `coord.dat` for an independent point colour scale.
#' @param points.scale.type Point scale type: "auto", "continuous", or "categorical".
#' @param points.levels Optional category order for a categorical point scale.
#' @param points.palette Colours for the point scale. Continuous scales take a colour vector;
#'   categorical scales take a colour vector, preferably named by category.
#' @param points.scale.label Legend title for the point scale (default is `points.col`).
#' @param points.min.value,points.max.value Limits for a continuous point scale (default data range).
#' @param points.intervals Tick spacing for a continuous point scale (default automatic).
#' @param points.legend.box Layout for the legends: "vertical" or "horizontal".
#' @return A ggplot object representing the map with matched points.
#' @details
#' With `points.col = NULL`, points use the matched HADCM3 values and the map's
#' colour scale. Setting `points.col` adds a separate continuous or categorical
#' point colour scale.
#' @export
#' @examples
#' HADCM3_exp_path <- PaleoClimR.example("HADCM3.permian.example")
#' data("PBDB.permian.example", package = "PaleoClimR")
#' HADCM3.points.map(
#'   var = "insitu_T_ym_dpth",
#'   file = "tfkSy_252o.pgclann",
#'   experiment = HADCM3_exp_path,
#'   depth.level = 1,
#'   coord.dat = PBDB.permian.example
#' )
HADCM3.points.map <- function(var,
                              file,
                              experiment,
                              depth.level = 1,
                              dims = NULL,
                              year = "default",
                              coord.dat,
                              lat.name = "p_lat",
                              lng.name = "p_lng",
                              max_dist = 1000,
                              na.method = "nearest",
                              unit.factor = NULL,
                              min.value = NULL,
                              max.value = NULL,
                              intervals = NULL,
                              continents.outlined = TRUE,
                              scale.label = NULL,
                              line.thickness = 1,
                              palette_name = pals::parula(1000),
                              projection = "ESRI:54012",
                              darkmode = FALSE,
                              custom.bg = NULL,
                              custom.fg = NULL,
                              col.labels = NULL,
                              show.NAs = TRUE,
                              points.col = NULL,
                              points.scale.type = c("auto", "continuous", "categorical"),
                              points.levels = NULL,
                              points.palette = NULL,
                              points.scale.label = NULL,
                              points.min.value = NULL,
                              points.max.value = NULL,
                              points.intervals = NULL,
                              points.legend.box = c("vertical", "horizontal")) {

  # Default values based on variable
  unit.factor <- if (is.null(unit.factor)) switch(var,
    "insitu_T_ym_dpth" = 1,
    1
  ) else unit.factor

  dims <- if (is.null(dims)) switch(var,
    "insitu_T_ym_dpth" = 3,
    3
  ) else dims

  min.value <- if (is.null(min.value)) switch(var,
    "insitu_T_ym_dpth" = 0,
    0
  ) else min.value

  max.value <- if (is.null(max.value)) switch(var,
    "insitu_T_ym_dpth" = 40,
    100
  ) else max.value

  intervals <- if (is.null(intervals)) switch(var,
    "insitu_T_ym_dpth" = 4,
    10
  ) else intervals

  scale.label <- if (is.null(scale.label)) switch(var,
    "insitu_T_ym_dpth" = "Temperature (\u00B0C)",
    "Variable"
  ) else scale.label

  if (is.null(col.labels)) col.labels <- seq(min.value, max.value, intervals)

  if (darkmode) {
    if (is.null(custom.bg)) custom.bg <- "black"
    if (is.null(custom.fg)) custom.fg <- "white"
  } else {
    if (is.null(custom.bg)) custom.bg <- "white"
    if (is.null(custom.fg)) custom.fg <- "black"
  }

  matched_points <- HADCM3.points.data(
    var = var,
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

  clim.map <- HADCM3.map(
    var = var,
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
    custom.bg = custom.bg,
    custom.fg = custom.fg,
    col.labels = col.labels
  )

  points.scale.type <- match.arg(points.scale.type)
  points.legend.box <- match.arg(points.legend.box)
  use.points.col <- !is.null(points.col)

  if (use.points.col) {
    if (!points.col %in% names(matched_points)) {
      stop("`points.col` ('", points.col, "') is not a column of `coord.dat`.")
    }
    if (!requireNamespace("ggnewscale", quietly = TRUE)) {
      stop("Package 'ggnewscale' is needed for `points.col`: install.packages('ggnewscale')")
    }

    if (points.scale.type == "auto") {
      points.scale.type <- if (is.numeric(matched_points[[points.col]])) "continuous" else "categorical"
    }
    if (is.null(points.scale.label)) points.scale.label <- points.col

    if (points.scale.type == "categorical") {
      raw <- coord.dat[[points.col]]
      raw.chr <- as.character(raw[!is.na(raw)])
      if (is.null(points.levels)) {
        points.levels <- if (is.factor(raw)) levels(raw) else unique(raw.chr)
      }
      unknown <- setdiff(unique(raw.chr), points.levels)
      if (length(unknown) > 0) {
        warning("Values not in `points.levels` will be plotted as NA: ",
                paste(unknown, collapse = ", "))
      }
    }
  }

  # Coordinates use the caller-selected columns, matching HADCM3.points.data().
  pts <- data.frame(
    lng = matched_points[[lng.name]],
    lat = matched_points[[lat.name]],
    matched_climate = matched_points$matched_climate * unit.factor
  )
  if (use.points.col) {
    pts$point_value <- matched_points[[points.col]]
    if (points.scale.type == "categorical") {
      pts$point_value <- factor(as.character(pts$point_value), levels = points.levels)
    }
  }

  colour.var <- if (use.points.col) "point_value" else "matched_climate"
  if (!show.NAs) pts <- pts[!is.na(pts[[colour.var]]), , drop = FALSE]

  points_sf <- sf::st_as_sf(pts, coords = c("lng", "lat"), crs = 4326)
  points_sf <- sf::st_transform(points_sf, projection)

  # Default behaviour: points use the map's HADCM3 colour scale.
  if (!use.points.col) {
    return(clim.map + ggplot2::geom_sf(
      data = points_sf,
      ggplot2::aes(geometry = geometry, fill = .data$matched_climate),
      shape = 21,
      size = 6,
      stroke = 1,
      alpha = 0.6
    ))
  }

  # Independent continuous or categorical point colour scale.
  if (points.scale.type == "continuous") {
    lo <- if (is.null(points.min.value)) min(pts$point_value, na.rm = TRUE) else points.min.value
    hi <- if (is.null(points.max.value)) max(pts$point_value, na.rm = TRUE) else points.max.value
    pal <- if (is.null(points.palette)) pals::viridis(1000) else points.palette
    fill.scale <- ggplot2::scale_fill_gradientn(
      colours = pal,
      limits = c(lo, hi),
      breaks = if (is.null(points.intervals)) ggplot2::waiver() else seq(lo, hi, points.intervals),
      oob = scales::squish,
      na.value = "grey50",
      name = points.scale.label,
      guide = ggplot2::guide_colourbar()
    )
  } else {
    pal <- if (is.null(points.palette)) scales::hue_pal()(length(points.levels)) else points.palette
    if (!is.null(names(pal)) && !all(points.levels %in% names(pal))) {
      stop("`points.palette` has no colour for: ",
           paste(setdiff(points.levels, names(pal)), collapse = ", "))
    }
    fill.scale <- ggplot2::scale_fill_manual(
      values = pal,
      limits = points.levels,
      drop = FALSE,
      na.value = "grey50",
      name = points.scale.label
    )
  }

  clim.map +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(
      data = points_sf,
      ggplot2::aes(geometry = geometry, fill = .data$point_value),
      shape = 21,
      size = 6,
      stroke = 1,
      alpha = 0.6,
      show.legend = TRUE
    ) +
    fill.scale +
    ggplot2::theme(legend.position = "bottom", legend.box = points.legend.box)
}
