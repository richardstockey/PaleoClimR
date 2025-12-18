#' Plot cGENIE .res Files as Time Series
#'
#' Imports a .res file, converts it to a data frame, and plots a specified column vs an x-axis variable.
#'
#' @param var Character; the variable file to import (e.g., "ocn_temp", "ocn_O2").
#' @param var_column Character; the column to plot on the y-axis (default: first data column excluding x_var).
#' @param x_var Character; the column to plot on the x-axis (default: "% time (yr)" if present, else first column).
#' @param experiment Character; path or name of the experiment.
#' @param line_color Character; color of the line/points (default "black").
#' @param line_size Numeric; thickness of the line/size of points (default 1).
#' @param base_size Numeric; base font size for the theme (default 24).
#' @param geom_type Character; type of geometry: "line", "point", or "both" (default "line").
#'
#' @return ggplot object representing the time series.
#' @export
cGENIE.res.plot <- function(var, var_column = NULL, x_var = NULL, experiment,
                            line_color = "black", line_size = 1, base_size = 24,
                            geom_type = "line") {

  # Import the data
  res.frame <- cGENIE.res.import(var = var, experiment = experiment)

  # Determine x-axis
  if (is.null(x_var)) {
    x_var <- if ("% time (yr)" %in% names(res.frame)) "% time (yr)" else names(res.frame)[1]
  }
  if (!x_var %in% names(res.frame)) stop("x_var not found in data: ", x_var)

  # Determine y-axis
  if (is.null(var_column)) {
    var_column <- names(res.frame)[which(names(res.frame) != x_var)[1]]  # first column that isn't x_var
  }
  if (!var_column %in% names(res.frame)) stop("var_column not found in data: ", var_column)

  # Start ggplot
  plot <- ggplot2::ggplot(res.frame, ggplot2::aes(x = .data[[x_var]], y = .data[[var_column]]))

  # Add geoms based on geom_type
  if (geom_type == "line") {
    plot <- plot + ggplot2::geom_line(color = line_color, size = line_size)
  } else if (geom_type == "point") {
    plot <- plot + ggplot2::geom_point(color = line_color, size = line_size)
  } else if (geom_type == "both") {
    plot <- plot + ggplot2::geom_line(color = line_color, size = line_size) +
      ggplot2::geom_point(color = line_color, size = line_size)
  } else {
    stop('geom_type must be "line", "point", or "both"')
  }

  # Add labels and theme
  plot <- plot +
    ggplot2::xlab(x_var) +
    ggplot2::ylab(var_column) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
      axis.line = ggplot2::element_line(lineend = 'square'),
      axis.text = ggplot2::element_text(color = "black"),
      legend.justification = c(1, 1),
      legend.position = c(.98, .36),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(plot)
}
