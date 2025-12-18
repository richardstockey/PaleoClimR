#' Generate a map from sf objects
#'
#' Creates a ggplot2 map from model data and optional continent outlines.
#'
#' @param data_sf sf object of the main data (grid cells with variable)
#' @param var_name character; name of the column in data_sf containing values
#' @param unit_factor numeric; scaling factor for var values
#' @param frame_sf sf object of the outer map frame
#' @param continents_sf sf object of continent outlines (optional)
#' @param fill_palette vector of colors for scale_fill_stepsn
#' @param min_value numeric; minimum for color scale
#' @param max_value numeric; maximum for color scale
#' @param intervals numeric; step size for color scale
#' @param scale_label character; label for color bar
#' @param outline_color character; color for continent outlines
#' @param frame_color character; color for map frame lines
#' @param fill_continents logical; whether to fill continents (default TRUE)
#' @param legend_position character; position of legend (default "bottom")
#' @param axis_color character; color for axis text and title
#' @param legend_text_color character; color for legend text
#' @param col_labels numeric; labels for the color bar (optional)
#' @return ggplot object representing the map
#' @export
paleo.map <- function(
    data_sf,
    var_name,
    unit_factor = 1,
    frame_sf,
    continents_sf = NULL,
    fill_palette = pals::parula(1000),
    min_value = 0,
    max_value = 100,
    intervals = 10,
    scale_label = "Variable",
    outline_color = "grey20",
    frame_color = "black",
    fill_continents = TRUE,
    legend_position = "bottom",
    axis_color = "black",
    legend_text_color = "black",
    col_labels = NULL
) {

  # Safety checks
  if (!var_name %in% names(data_sf)) stop("var_name not found in data_sf")
  if(is.null(col_labels)) col_labels <- seq(min_value, max_value, intervals)

  # Start ggplot
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = .data[[var_name]] * unit_factor),
      color = NA
    ) +
    ggplot2::geom_sf(
      data = frame_sf,
      color = frame_color,
      linewidth = 1,
      fill = NA
    )

  # Add continents if provided
  if (!is.null(continents_sf) && fill_continents) {
    map <- map + ggplot2::geom_sf(
      data = continents_sf,
      fill = "grey80",
      color = outline_color,
      linewidth = 1
    )
  }

  # Color scale
  map <- map + ggplot2::scale_fill_stepsn(
    colours = fill_palette,
    breaks = col_labels,
    limits = c(min_value, max_value),
    labels = col_labels,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barwidth = 12,
      barheight = 1,
      frame.colour = frame_color,
      ticks.colour = frame_color
    )
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend_position,
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.text = ggplot2::element_text(color = legend_text_color),
      legend.title = ggplot2::element_text(color = legend_text_color),
      axis.text = ggplot2::element_text(color = axis_color),
      axis.title = ggplot2::element_text(color = axis_color)
    ) +
    ggplot2::labs(fill = scale_label)

  return(map)
}
