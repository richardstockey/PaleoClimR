#' Plot a Transect from Gridded Data
#'
#' Utility function to create a latitude or longitude transect plot
#' from pre-filtered gridded data (e.g. using gen.transect.slice).
#'
#' @param df.slice Filtered dataframe for the transect.
#' @param orientation "lat" or "lon".
#' @param box Dataframe defining plot bounding box.
#' @param land_segments Dataframe of land outline segments.
#' @param unit.factor Numeric multiplier for variable scaling.
#' @param palette_name Colour palette.
#' @param min.value Minimum colour scale value.
#' @param max.value Maximum colour scale value.
#' @param intervals Interval for colour breaks.
#' @param custom.fg Foreground colour.
#' @param upper.bound Upper y-axis bound.
#' @param scale.label Legend label.
#' @param title_text Plot title.
#'
#' @return ggplot object
#' @export
paleo.transect <- function(df.slice,
                           orientation = "lat",
                           box,
                           land_segments,
                           unit.factor = 1,
                           palette_name,
                           min.value,
                           max.value,
                           intervals,
                           custom.fg,
                           upper.bound,
                           scale.label,
                           title_text) {

  if (!orientation %in% c("lat", "lon")) {
    stop("orientation must be 'lat' or 'lon'")
  }

  # ---- Orientation-specific mappings ----
  if (orientation == "lat") {
    xlab <- "Latitude (\u00B0)"
    xmin_col <- "lat.min"
    xmax_col <- "lat.max"
    x_limits <- c(-90, 90)
    x_breaks <- pretty(x_limits)
  } else {
    xlab <- "Longitude (\u00B0)"
    xmin_col <- "lon.min"
    xmax_col <- "lon.max"
    x_limits <- c(-180, 180)
    x_breaks <- pretty(x_limits)
  }

  # ---- Core plot ----
  transect <- ggplot2::ggplot(
    data = df.slice,
    ggplot2::aes(
      xmin = .data[[xmin_col]],
      xmax = .data[[xmax_col]],
      ymin = .data[["depth.min"]],
      ymax = .data[["depth.max"]],
      fill = .data[["var"]] * unit.factor
    )
  ) +
    ggplot2::geom_rect(color = NA, linewidth = 10) +
    ggplot2::geom_rect(
      data = box,
      ggplot2::aes(
        xmin = .data[["xmin"]],
        xmax = .data[["xmax"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]]
      ),
      color = custom.fg,
      linewidth = 1,
      fill = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = land_segments,
      ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        xend = .data[["xend"]],
        yend = .data[["yend"]]
      ),
      color = custom.fg,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_stepsn(
      colours = palette_name,
      breaks = seq(min.value, max.value, intervals),
      limits = c(min.value, max.value),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth = 12,
        barheight = 1,
        raster = FALSE,
        frame.colour = custom.fg,
        ticks.colour = custom.fg
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_reverse(
      limits = c(max(df.slice$`depth.max`), upper.bound),
      breaks = pretty(c(0, max(df.slice$`depth.max`))),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      expand = c(0, 0)
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 18, colour = custom.fg),
      axis.title = ggplot2::element_text(size = 18, colour = custom.fg),
      axis.text = ggplot2::element_text(size = 18, colour = custom.fg)
    ) +
    ggplot2::ylab("Depth (m)") +
    ggplot2::xlab(xlab) +
    ggplot2::labs(fill = scale.label, title = title_text)

  return(transect)
}
