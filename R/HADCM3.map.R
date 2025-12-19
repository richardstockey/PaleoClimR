#' Generate Maps from HADCM3 Model Output (as formatted by the BRIDGE Group)
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file. (e.g. "insitu_T_ym_dpth")
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param depth.level (numeric) Depth layer to visualize (default = 1).
#' @param dims (numeric) Dimensionality of the data (default NULL; auto-set by var).
#' @param year (numeric or character) Time step to visualize (default "default").
#' @param unit.factor (numeric) Scaling factor for variable (default NULL; auto-set by var).
#' @param min.value,max.value (numeric) Color scale limits (default NULL; auto-set by var).
#' @param intervals (numeric) Color scale intervals (default NULL; auto-set by var).
#' @param continents.outlined (logical) Whether to outline continents (default TRUE).
#' @param scale.label (character) Label for color scale (default NULL; auto-set by var).
#' @param line.thickness (numeric) Thickness of continent outlines (default 1).
#' @param palette_name (character) Color palette for the plot (default pals::parula(1000)).
#' @param projection (character) Map projection (default 'ESRI:54012').
#' @param darkmode (logical) Enable dark mode (default FALSE).
#' @param custom.bg (character) Background color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param custom.fg (character) Foreground color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param col.labels (numeric) Labels for the color bar ticks.
#' @return A ggplot object representing the map.
#' @export
HADCM3.map <- function(var, file,
                       experiment,
                       depth.level = 1,
                       dims = NULL,
                       year = "default",
                       unit.factor = NULL,
                       min.value = NULL,
                       max.value = NULL,
                       intervals = NULL,
                       continents.outlined = TRUE,
                       scale.label = NULL,
                       line.thickness = 1,
                       palette_name = pals::parula(1000),
                       projection = 'ESRI:54012',
                       darkmode = FALSE,
                       custom.bg = NULL,
                       custom.fg = NULL,
                       col.labels = NULL) {

  # --- Default values based on variable ---
  unit.factor <- if(is.null(unit.factor)) switch(var,
                                                 "insitu_T_ym_dpth" = 1,
                                                 1
  ) else unit.factor

  dims <- if(is.null(dims)) switch(var,
                                   "insitu_T_ym_dpth" = 3,
                                   3
  ) else dims

  min.value <- if(is.null(min.value)) switch(var,
                                             "insitu_T_ym_dpth" = 0,
                                             0
  ) else min.value

  max.value <- if(is.null(max.value)) switch(var,
                                             "insitu_T_ym_dpth" = 40,
                                             100
  ) else max.value

  intervals <- if(is.null(intervals)) switch(var,
                                             "insitu_T_ym_dpth" = 4,
                                             10
  ) else intervals

  scale.label <- if(is.null(scale.label)) switch(var,
                                                 "insitu_T_ym_dpth" = "Temperature (\u00B0C)",
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

  # --- Load & clean data ---
  df <- HADCM3.data(var = var,
                    file = file,
                    experiment = experiment,
                    depth.level = depth.level,
                    dims = dims,
                    year = year)

  df <- clean.nc.df(df = df)
  SpDfSf <- nc.df.to.sf(df)
  SLs1dfSf <- gen.map.frame.sf()
  continents_proj <- if(continents.outlined) sf::st_transform(gen.continents.sf(df), projection) else NULL

  # Transform all spatial layers once
  data_proj <- sf::st_transform(SpDfSf, projection)
  frame_proj <- sf::st_transform(SLs1dfSf, projection)
  if(!is.null(continents_proj)) continents_proj <- sf::st_transform(continents_proj, projection)

  # --- Generate map ---
  map <- paleo.map(
    data_sf = data_proj,
    var_name = "var",
    unit_factor = unit.factor,
    frame_sf = frame_proj,
    continents_sf = continents_proj,
    fill_palette = palette_name,
    min_value = min.value,
    max_value = max.value,
    intervals = intervals,
    scale_label = scale.label,
    outline_color = custom.fg,
    frame_color = custom.fg,
    fill_continents = !is.null(continents_proj),
    col_labels = col.labels,
    axis_color = custom.fg,
    legend_text_color = custom.fg
  )

  # Apply darkmode background
  map <- map + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = custom.bg),
    plot.background  = ggplot2::element_rect(fill = custom.bg)
  )

  return(map)
}
