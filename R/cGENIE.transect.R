#' Create Transect Plots from cGENIE Model Output
#'
#' This function creates transect plots from cGENIE model output .nc files.
#' 
#' @param var (character) Variable to visualize (e.g. "ocn_temp", "ocn_sal", "ocn_O2").
#' @param experiment (character) Path or name of the experiment.
#' @param depth.level (numeric) Depth layer to visualize (default = 1).
#' @param dims (numeric) Dimensionality of the data (default NULL; auto-set by var).
#' @param year (numeric or character) Time step to visualize (default "default").
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
#' @param upper.bound Numeric value indicating the upper bound for the y axis. Defaults to -20 to avoid floating y axis ticks. Can be tweaked for aesthetics.
#' @param lower.bound Numeric value indicating the lower bound for the y axis. Defaults to "default", which sets it to the maximum depth in the data.
#' @return A ggplot object representing the transect plot.
#' @export

cGENIE.transect <- function(experiment, var,
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
        model = "biogem",
        palette_name = pals::parula(1000),
        projection = 'ESRI:54012',
        darkmode = FALSE,
        custom.bg = NULL,
        custom.fg = NULL,
        col.labels = NULL,
        upper.bound = -20,
        lower.bound = "default"
        ){

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
                                             "ocn_O2"   = 300,
                                             "grid_topo" = 0,
                                             "phys_psi" = 75,
                                             100
  ) else max.value

  intervals <- if(is.null(intervals)) switch(var,
                                             "ocn_temp" = 4,
                                             "ocn_sal"  = 1,
                                             "ocn_H2S"  = 4,
                                             "ocn_O2"   = 25,
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

  # --- Load & clean data ---
  df <- cGENIE.data.3D(var = var,
                    experiment = experiment,
                    year = year,
                    model = model)

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
    title_text = title_text
  )
    # Return the plot
  transect
}
