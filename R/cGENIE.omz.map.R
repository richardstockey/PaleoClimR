#' Map the thickness of oxygen minimum zones in a cGENIE model
#'
#' Plots the total thickness (m) of all cells classified as oxygen minimum zone (OMZ) in each
#' lat/lon water column, using \code{cGENIE.omz.thickness()}. Land columns are left blank; ocean
#' columns with no OMZ have a thickness of 0.
#'
#' @param experiment (character) Path or name of the experiment.
#' @param OMZ.thresh (numeric) Oxygen concentration threshold (mol/kg) at or below which a cell is
#'   classed as OMZ. Default is 58.54e-6 (i.e. 58.54 umol/kg), based on Canfield 60uM.
#' @param year (numeric or character) Time step to visualize (default "default").
#' @param min.value,max.value (numeric) Color scale limits in metres (default min = 0; max = NULL,
#'   set automatically from the data).
#' @param intervals (numeric) Color scale intervals in metres (default NULL; set automatically
#'   from the data).
#' @param continents.outlined (logical) Whether to outline continents (default TRUE).
#' @param scale.label (character) Label for color scale (default "OMZ thickness (m)").
#' @param line.thickness (numeric) Thickness of continent outlines (default 1).
#' @param palette_name (character) Color palette for the plot (default pals::parula(1000)).
#' @param projection (character) Map projection (default 'ESRI:54012').
#' @param darkmode (logical) Enable dark mode (default FALSE).
#' @param custom.bg (character) Background color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param custom.fg (character) Foreground color for dark mode (default NULL; auto-set if darkmode = TRUE).
#' @param col.labels (numeric) Labels for the color bar ticks.
#' @return A ggplot object representing the map.
#' @export
#' @examples
#' cGENIE_exp_path <- PaleoClimR.example("cGENIE.permian.example")
#' cGENIE.omz.map(
#'   experiment = cGENIE_exp_path,
#' )
cGENIE.omz.map <- function(experiment,
                           OMZ.thresh = 58.54e-6,
                           year = "default",
                           min.value = 0,
                           max.value = NULL,
                           intervals = NULL,
                           continents.outlined = TRUE,
                           scale.label = "OMZ thickness (m)",
                           line.thickness = 1,
                           palette_name = pals::parula(1000),
                           projection = 'ESRI:54012',
                           darkmode = FALSE,
                           custom.bg = NULL,
                           custom.fg = NULL,
                           col.labels = NULL) {

  # --- OMZ thickness for each lat/lon column ---
  df <- cGENIE.omz.thickness(experiment = experiment,
                             OMZ.thresh = OMZ.thresh,
                             year = year)
  df <- as.data.frame(df)
  df$var <- df$omz.thickness                 # paleo.map plots the "var" column
  df <- df[order(df$lat.mid, df$lon.mid), ]  # lat-major order, as in the cGENIE.data output
  rownames(df) <- NULL

  # Color scale defaults from the data
  data.max <- suppressWarnings(max(df$var, na.rm = TRUE))
  if (!is.finite(data.max) || data.max <= 0) {
    message("No OMZ cells found at this threshold; using an empty 0-100 m scale.")
    data.max <- 100
  }
  if (is.null(intervals)) intervals <- diff(pretty(c(min.value, data.max), n = 8))[1]
  if (is.null(max.value)) max.value <- ceiling(data.max / intervals) * intervals

  # Color bar labels
  if (is.null(col.labels)) col.labels <- seq(min.value, max.value, intervals)

  # Dark mode defaults
  if (darkmode) {
    if (is.null(custom.bg)) custom.bg <- "black"
    if (is.null(custom.fg)) custom.fg <- "white"
  } else {
    if (is.null(custom.bg)) custom.bg <- "white"
    if (is.null(custom.fg)) custom.fg <- "black"
  }

  # --- Clean data & build spatial layers ---
  df <- clean.nc.df(df = df)
  SpDfSf <- nc.df.to.sf(df)
  SLs1dfSf <- gen.map.frame.sf()
  continents_proj <- if (continents.outlined) {
    sf::st_transform(gen.continents.sf(df), projection)
  } else NULL

  data_proj  <- sf::st_transform(SpDfSf, projection)
  frame_proj <- sf::st_transform(SLs1dfSf, projection)

  # --- Generate map ---
  map <- paleo.map(
    data_sf = data_proj,
    var_name = "var",
    unit_factor = 1,                         # thickness is already in metres
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
