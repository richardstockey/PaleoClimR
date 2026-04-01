#' Create Transect Plots from HADCM3 Model Output (as formatted by the BRIDGE Group)
#'
#' This function creates transect plots from HADCM3 model output .nc files.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file. (e.g. "insitu_T_ym_dpth")
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param orientation Character. Whether the transect is oriented along latitude ("lat") or longitude ("lon"). Default is "lat".
#' @param degrees Numeric. The degree band of the specified latitude or longitude to include in the transect (default 0, meaning the equator/dateline). Will plot the nearest band to your chosen value. 
#' @param dims (numeric) Dimensionality of the data (almost certainly 3 if you're plotting a transect, set to 3 as default).
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
#' @param upper.bound Numeric value indicating the upper bound for the y axis. Defaults to -20 to avoid floating y axis ticks. Can be tweaked for aesthetics.
#' @param lower.bound Numeric value indicating the lower bound for the y axis. Defaults to "default", which sets it to the maximum depth in the data.
#' @return A ggplot object representing the transect plot.
#' @export

HADCM3.transect <- function(var, file,
                            experiment,
        degrees = 0,
        dims = 3,
        orientation = "lat",
        year = "default",
        unit.factor = 1,
        min.value = 0,
        max.value = 40,
        intervals = 5,
        continents.outlined,
        scale.label = NULL,
        line.thickness = 1,
        palette_name = pals::parula(1000),
        projection = 'ESRI:54012',
        darkmode = FALSE,
        custom.bg = NULL,
        custom.fg = NULL,
        col.labels = NULL,
        upper.bound = -20,
        lower.bound = "default"
        ){

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
  df <- HADCM3.data.3D(var = var,
                       file = file,
                       experiment = experiment,
                    year = year)

  df <- clean.nc.df(df = df)

  if(lower.bound == "default"){
  lower.bound <- max(df$depth.max)
  }else{}

  slice <- gen.transect.slice(
    df = df,
    degrees = degrees,
    orientation = orientation,
    upper.bound = upper.bound,
    lower.bound = lower.bound
  )

  df.slice <- slice$df.slice
  box <- slice$box
  box.xmin <- slice$xlims[1]
  box.xmax <- slice$xlims[2]

  if (orientation == "lat") {
    title_text <- paste0(slice$band.label, "\u00B0 Longitude")
  } else {
    title_text <- paste0(slice$band.label, "\u00B0 Latitude")
  }

  # generate outline of continents for aesthetic purposes
  land_segments <- gen.continents.transect(
    df.slice,
    var = "var",
    orientation = orientation
  )

  transect <- paleo.transect(
    df.slice = df.slice,
    orientation = orientation,
    box = box,
    land_segments = land_segments,
    unit.factor = unit.factor,
    palette_name = palette_name,
    min.value = min.value,
    max.value = max.value,
    intervals = intervals,
    custom.fg = custom.fg,
    upper.bound = upper.bound,
    scale.label = scale.label,
    title_text = title_text, 
    line.thickness = line.thickness
  )
    # Return the plot
  transect
}
