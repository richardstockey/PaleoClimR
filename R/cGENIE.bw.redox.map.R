#' Map Bottom-Water Redox Conditions
#'
#' Maps the redox classification of every cGENIE bottom-water grid cell using
#' benthic oxygen output. Cells are classified as anoxic, suboxic, or oxic.
#'
#' @param experiment A character string specifying the path to the cGENIE experiment folder.
#' @param anox.thresh Numeric oxygen threshold (mol/kg) for anoxic bottom water. Default is 0.
#' @param subox.thresh Numeric oxygen threshold (mol/kg) for suboxic bottom water. Default is 4.8e-6.
#' @param anox.col,subox.col,ox.col Colours for the anoxic, suboxic, and oxic classes.
#' @param class.labels Character vector of legend labels, in anoxic, suboxic, oxic order.
#' @param continents.outlined Logical; whether to draw continent outlines (default TRUE).
#' @param model Character model type passed to [cGENIE.data] (default "biogem").
#' @param line.thickness Numeric thickness of continent outlines (default 1).
#' @param projection Map projection (default "ESRI:54012").
#' @param darkmode Logical; use a dark background (default FALSE).
#' @param custom.bg,custom.fg Optional background and foreground colours.
#' @param legend.title Legend title (default "Bottom-water redox").
#' @return A ggplot object showing the bottom-water redox class of each ocean grid cell.
#'
#' @details Oxygen concentrations are classified as anoxic when they are less than
#' or equal to `anox.thresh`, suboxic when greater than `anox.thresh` and less than
#' or equal to `subox.thresh`, and oxic when greater than `subox.thresh`. The legend
#' always includes all three classes, including classes with no grid cells.
#'
#' @export
#' @examples
#' cGENIE_exp_path <- PaleoClimR.example("cGENIE.permian.example")
#' cGENIE.bw.redox.map(cGENIE_exp_path)
cGENIE.bw.redox.map <- function(experiment,
                               anox.thresh = 0,
                               subox.thresh = 4.8e-6,
                               anox.col = "#b2182b",
                               subox.col = "#fdae61",
                               ox.col = "#2166ac",
                               class.labels = c("Anoxic", "Suboxic", "Oxic"),
                               continents.outlined = TRUE,
                               model = "biogem",
                               line.thickness = 1,
                               projection = "ESRI:54012",
                               darkmode = FALSE,
                               custom.bg = NULL,
                               custom.fg = NULL,
                               legend.title = "Bottom-water redox") {

  if (length(anox.thresh) != 1 || is.na(anox.thresh) ||
      length(subox.thresh) != 1 || is.na(subox.thresh) ||
      anox.thresh >= subox.thresh) {
    stop("`anox.thresh` and `subox.thresh` must be single values with anox.thresh < subox.thresh.")
  }
  if (length(class.labels) != 3) {
    stop("`class.labels` must contain exactly three labels: anoxic, suboxic, and oxic.")
  }

  if (darkmode) {
    if (is.null(custom.bg)) custom.bg <- "black"
    if (is.null(custom.fg)) custom.fg <- "white"
  } else {
    if (is.null(custom.bg)) custom.bg <- "white"
    if (is.null(custom.fg)) custom.fg <- "black"
  }

  # Read benthic oxygen and classify each grid cell using the summary function's thresholds.
  bw.o2 <- cGENIE.data(experiment = experiment,
                       var = "ocn_ben_O2",
                       dims = 2,
                       year = "default",
                       model = model)

  bw.redox <- bw.o2 %>%
    dplyr::mutate(redox.class = dplyr::case_when(
      .data$var <= anox.thresh ~ "anoxic",
      .data$var <= subox.thresh ~ "suboxic",
      .data$var > subox.thresh ~ "oxic",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(redox.class = factor(.data$redox.class,
                                       levels = c("anoxic", "suboxic", "oxic")))

  bw.clean <- clean.nc.df(df = bw.redox)
  bw.sf <- nc.df.to.sf(bw.clean)
  # nc.df.to.sf preserves the numeric field as `var`; attach the factor separately
  # so the categorical values survive polygon conversion (including dateline splits).
  bw.sf$redox.class <- bw.clean$redox.class
  frame.sf <- gen.map.frame.sf()
  continents.sf <- if (continents.outlined) gen.continents.sf(bw.clean) else NULL

  bw.sf <- sf::st_transform(bw.sf, projection)
  frame.sf <- sf::st_transform(frame.sf, projection)
  if (!is.null(continents.sf)) continents.sf <- sf::st_transform(continents.sf, projection)

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = bw.sf,
                     ggplot2::aes(fill = .data$redox.class),
                     color = NA,
                     show.legend = TRUE) +
    ggplot2::geom_sf(data = frame.sf,
                     color = custom.fg,
                     linewidth = line.thickness,
                     fill = NA)

  if (!is.null(continents.sf)) {
    map <- map + ggplot2::geom_sf(data = continents.sf,
                                  fill = "grey80",
                                  color = custom.fg,
                                  linewidth = line.thickness)
  }

  map <- map +
    ggplot2::scale_fill_manual(
      values = c(anoxic = anox.col, suboxic = subox.col, oxic = ox.col),
      limits = c("anoxic", "suboxic", "oxic"),
      breaks = c("anoxic", "suboxic", "oxic"),
      labels = class.labels,
      drop = FALSE,
      na.value = "transparent",
      name = legend.title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = custom.bg, color = NA),
      plot.background = ggplot2::element_rect(fill = custom.bg, color = NA),
      legend.background = ggplot2::element_rect(fill = custom.bg, color = NA),
      legend.text = ggplot2::element_text(color = custom.fg),
      legend.title = ggplot2::element_text(color = custom.fg),
      axis.text = ggplot2::element_text(color = custom.fg),
      axis.title = ggplot2::element_text(color = custom.fg)
    )

  return(map)
}
