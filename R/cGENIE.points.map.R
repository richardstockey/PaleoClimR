#' Generate Maps from cGENIE Model Output with Points Matching
#'
#' This function generates maps from imported .nc (NetCDF) files containing data from the cGENIE model outputs.
#' It can handle both 2D and 3D data, visualizing variables across specified depth levels and time steps.
#' Additionally, it matches and plots specific points from a provided data frame.
#'
#' @param var (character) Variable to visualize (e.g. "ocn_temp", "ocn_sal", "ocn_O2").
#' @param experiment (character) Path or name of the experiment.
#' @param depth.level (numeric) Depth layer to visualize (default = 1).
#' @param dims (numeric) Dimensionality of the data (default NULL; can be auto-set by var).
#' @param year (numeric or character) Time step to visualize (default "default").
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#' @param max_dist Maximum allowed distance for matching (km, default 1000 km).
#' @param na.method How to handle NA climate values: "nearest" (default), "same.lat", "keep".
#' @param unit.factor (numeric) Scaling factor for variable (default NULL; auto-set by var).
#' @param min.value,max.value (numeric) Color scale limits (default NULL; auto-set by var).
#' @param intervals (numeric) Color scale intervals (default NULL; auto-set by var).
#' @param continents.outlined (logical) Whether to outline continents (default TRUE).
#' @param scale.label (character) Label for color scale (default NULL; auto-set by var).
#' @param model (character) Model type (default "biogem").
#' @param line.thickness (numeric) Thickness of continent outlines (default 1).
#' @param palette_name (character) Color palette for the plot (default pals::parula(1000)).
#' @param projection (character) Map projection (default 'ESRI:54012').
#' @param darkmode (logical) Enable dark mode (default FALSE).
#' @param custom.bg (character) Background color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param custom.fg (character) Foreground color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param col.labels (numeric) Labels for the color bar ticks.
#' @param show.NAs (logical) Whether to show NA values on the map (default FALSE).
#' @param points.col (character) Name of a column in `coord.dat` used to colour the points on a second,
#'   independent colour scale. Default NULL: points are coloured by the matched cGENIE value on the map's scale.
#' @param points.scale.type (character) "auto" (default; numeric columns are continuous, everything else
#'   categorical), "continuous" or "categorical". Use "categorical" to force e.g. 0/1 codes to be treated as categories.
#' @param points.levels (character) Categories of a categorical second scale, in the order they should appear
#'   in the legend. Default NULL: the levels of the column if it is a factor, otherwise the order of first
#'   appearance in `coord.dat`. All categories are always shown in the legend, even if no plotted point has
#'   that value.
#' @param points.palette Colours for the second scale. Continuous: a vector of colours (default pals::viridis(1000)).
#'   Categorical: a (preferably named) vector with a colour for every category, e.g.
#'   c(oxic = "steelblue", anoxic = "firebrick") (default: evenly spaced hues in the order of the levels).
#' @param points.scale.label (character) Legend title for the second scale (default: `points.col`).
#' @param points.min.value,points.max.value (numeric) Limits of a continuous second scale (default: range of the data).
#'   Values outside the limits are shown in the end colours.
#' @param points.intervals (numeric) Tick spacing for a continuous second scale (default NULL: automatic).
#' @param points.legend.box (character) "vertical" (default; second scale below the map's scale) or
#'   "horizontal" (side by side).
#' @return A ggplot object representing the generated map with the specified variable visualized across geographical coordinates.
#'
#' @details
#' This function reads 2D or 3D data from cGENIE model output NetCDF files and produces a map visualization.
#' Default settings are defined for several commonly used variables, and users can specify their own scaling and color settings.
#' The function also matches specific points from a provided data frame and plots them on the map.
#'
#' @export
#' @examples
#' cGENIE_exp_path <- PaleoClimR.example("cGENIE.permian.example")
#' data("PBDB.permian.example", package = "PaleoClimR")
#' cGENIE.points.map(
#'   var = "ocn_O2",
#'   experiment = cGENIE_exp_path,
#'   depth.level = 2,
#'   coord.dat = PBDB.permian.example
#' )
cGENIE.points.map <- function(var, experiment,
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
                              show.NAs = TRUE,
                              points.col = NULL,
                              points.scale.type = c("auto", "continuous", "categorical"),
                              points.levels = NULL,
                              points.palette = NULL,
                              points.scale.label = NULL,
                              points.min.value = NULL,
                              points.max.value = NULL,
                              points.intervals = NULL,
                              points.legend.box = c("vertical", "horizontal")){

  # Set default values based on variable
  unit.factor <- if(is.null(unit.factor)) switch(var,
                                                 "ocn_temp" = 1,
                                                 "ocn_sal"  = 1,
                                                 "ocn_H2S"  = 1e6,
                                                 "ocn_O2"   = 1e6,
                                                 "grid_topo" = -1,
                                                 "phys_psi" = 1,
                                                 1
  ) else unit.factor

  dims <- if(is.null(dims)) switch(var,
                                   "ocn_temp" = 3,
                                   "ocn_sal"  = 3,
                                   "ocn_H2S"  = 3,
                                   "ocn_O2"   = 3,
                                   "grid_topo" = 2,
                                   "phys_psi" = 2,
                                   3
  ) else dims

  min.value <- if(is.null(min.value)) switch(var,
                                             "ocn_temp" = 0,
                                             "ocn_sal"  = 30,
                                             "ocn_H2S"  = 0,
                                             "ocn_O2"   = 0,
                                             "grid_topo" = -5000,
                                             "phys_psi" = -75,
                                             0
  ) else min.value

  max.value <- if(is.null(max.value)) switch(var,
                                             "ocn_temp" = 40,
                                             "ocn_sal"  = 40,
                                             "ocn_H2S"  = 40,
                                             "ocn_O2"   = 320,
                                             "grid_topo" = 0,
                                             "phys_psi" = 75,
                                             100
  ) else max.value

  intervals <- if(is.null(intervals)) switch(var,
                                             "ocn_temp" = 4,
                                             "ocn_sal"  = 1,
                                             "ocn_H2S"  = 4,
                                             "ocn_O2"   = 40,
                                             "grid_topo" = 500,
                                             "phys_psi" = 15,
                                             10
  ) else intervals

  scale.label <- if(is.null(scale.label)) switch(var,
                                                 "ocn_temp" = "Temperature (\u00B0C)",
                                                 "ocn_sal"  = "Salinity (PSU)",
                                                 "ocn_H2S"  = "Hydrogen Sulfide (\u03BCmol/kg)",
                                                 "ocn_O2"   = "Oxygen (\u00B5mol/kg)",
                                                 "grid_topo" = "Ocean Depth (m)",
                                                 "phys_psi" = "Barotropic streamfunction (Sv)",
                                                 "Variable"
  ) else scale.label

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

  matched_points <- cGENIE.points.data(var = var,
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


  clim.map <- cGENIE.map(var = var,
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
                         model = model,
                         line.thickness = line.thickness,
                         palette_name = palette_name,
                         projection = projection,
                         darkmode = darkmode,
                         col.labels = col.labels)

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
      # Categories come from the ORIGINAL column, so every category is kept even if all of its
      # rows are dropped later (missing coordinates, show.NAs = FALSE)
      raw     <- coord.dat[[points.col]]
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

  # Plain data frame of point positions and the value(s) used for colour
  pts <- data.frame(
    lng = matched_points[[lng.name]],
    lat = matched_points[[lat.name]],
    matched_climate = matched_points$matched_climate * unit.factor
  )
  if (use.points.col) {
    pts$point_value <- matched_points[[points.col]]
    if (points.scale.type == "categorical") {
      # fixed level order, including levels with no points
      pts$point_value <- factor(as.character(pts$point_value), levels = points.levels)
    }
  }

  # show.NAs = FALSE drops points with NA in the value used for colour
  colour.var <- if (use.points.col) "point_value" else "matched_climate"
  if (!show.NAs) pts <- pts[!is.na(pts[[colour.var]]), , drop = FALSE]

  points_sf <- sf::st_as_sf(pts, coords = c("lng", "lat"), crs = 4326)
  points_sf <- sf::st_transform(points_sf, projection)

  # --- Original behaviour: points coloured on the map's scale ---
  if (!use.points.col) {
    map.points <- clim.map +
      ggplot2::geom_sf(data = points_sf,
                       ggplot2::aes(geometry = geometry, fill = .data$matched_climate),
                       shape = 21, size = 6, stroke = 1.0, alpha = 0.6)
    return(map.points)
  }

  # --- Second colour scale ---
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
      guide = ggplot2::guide_colourbar()  # adjust size/ticks here to match the map's colour bar
    )
  } else {
    pal <- if (is.null(points.palette)) scales::hue_pal()(length(points.levels)) else points.palette
    if (!is.null(names(pal)) && !all(points.levels %in% names(pal))) {
      stop("`points.palette` has no colour for: ",
           paste(setdiff(points.levels, names(pal)), collapse = ", "))
    }
    fill.scale <- ggplot2::scale_fill_manual(
      values = pal,
      limits = points.levels,   # all categories, in order, even if absent from the plotted points
      drop = FALSE,
      na.value = "grey50",
      name = points.scale.label
    )
  }

  map.points <- clim.map +
    ggnewscale::new_scale_fill() +          # everything after this uses a new, independent fill scale
    ggplot2::geom_sf(data = points_sf,
                     ggplot2::aes(geometry = geometry, fill = .data$point_value),
                     shape = 21, size = 6, stroke = 1.0, alpha = 0.6,
                     show.legend = TRUE) +  # draw a key for every level, even ones with no points
    fill.scale +
    ggplot2::theme(legend.position = "bottom", legend.box = points.legend.box)

  return(map.points)
}
