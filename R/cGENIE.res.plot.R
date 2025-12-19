#' Plot cGENIE .res File Variables
#'
#' Imports .res files, converts to data frame, and plots as a time series or custom x/y variable.
#'
#' @param var Character; variable name to import (e.g., "ocn_temp", "ocn_O2").
#' @param experiment Character; experiment directory.
#' @param var_col_no Numeric; column index for y-axis variable (default = 2, first column after time).
#' @param x_var Character; column name for x-axis (default = "% time (yr)").
#' @param plot_type Character; "line", "dots", or "both" (default = "line").
#' @param base_size Numeric; base font size for theme (default = 14).
#'
#' @return ggplot object.
#' @import ggplot2
#' @export
cGENIE.res.plot <- function(var,
                            experiment,
                            var_col_no = 2,
                            x_var = "% time (yr)",
                            plot_type = "line",
                            base_size = 14) {

  # Import data
  res.frame <- cGENIE.res.import(var = var, experiment = experiment)

  # Validate x_var
  if(!x_var %in% names(res.frame)) stop("x_var not found in data: ", x_var)

  # Determine y-axis column name from index
  if(!is.numeric(var_col_no) || var_col_no < 1 || var_col_no > ncol(res.frame)) {
    stop("var_col_no out of bounds")
  }
  y_var <- names(res.frame)[var_col_no]

  # Base ggplot
  plot <- ggplot2::ggplot(res.frame, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))

  # Add plot type
  if(plot_type == "line") {
    plot <- plot + ggplot2::geom_line()
  } else if(plot_type == "dots") {
    plot <- plot + ggplot2::geom_point()
  } else if(plot_type == "both") {
    plot <- plot + ggplot2::geom_line() + ggplot2::geom_point()
  } else {
    stop("plot_type must be 'line', 'dots', or 'both'")
  }

  # Theme and labels
  plot <- plot +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::xlab(x_var) +
    ggplot2::ylab(y_var) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
      axis.line = ggplot2::element_line(lineend = 'square'),
      axis.text = ggplot2::element_text(color = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(plot)
}
