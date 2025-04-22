#' cGENIE.res.plot
#'
#' This function imports .res files and converts them into data frames, then plots them as time series.
#' It builds on the `cGENIE.res.import` function.
#'
#' @param var A character string specifying the variable to be plotted (e.g., "ocn_temp", "ocn_O2", "atm_pO2", "atm_pCO2").
#' @param sub_var A character string specifying the sub-variable to be plotted. Default is "default".
#' @param experiment A character string specifying the experiment name.
#'
#' @return A ggplot object representing the time series plot of the specified variable.
#' 
#' @import ggplot2
#' @export
cGENIE.res.plot <- function(var, sub_var = "default", experiment){
  # Import the data using the cGENIE.res.import function
  res.frame <- cGENIE.res.import(var = var, experiment = experiment)
  
  # Check if sub_var is set to "default"
  if(sub_var == "default"){
    # Plot for ocean temperature
    if(var == "ocn_temp"){
      plot <- ggplot2::ggplot(data = res.frame, ggplot2::aes(x = `% time (yr)` , y = `_surT (ice-free) (C)`)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(24) +
        ggplot2::xlab("Time (yr)") +
        ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = ggplot2::element_line(lineend = 'square'),
              axis.text = ggplot2::element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank())
    
    # Plot for ocean oxygen
    } else if(var == "ocn_O2"){
      plot <- ggplot2::ggplot(data = res.frame, ggplot2::aes(x = `% time (yr)` , y = `global mean O2 (mol kg-1)`)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(24) +
        ggplot2::xlab("Time (yr)") +
        ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = ggplot2::element_line(lineend = 'square'),
              axis.text = ggplot2::element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank())
    
    # Plot for atmospheric oxygen partial pressure
    } else if(var == "atm_pO2"){
      plot <- ggplot2::ggplot(data = res.frame, ggplot2::aes(x = `% time (yr)` , y = `global pO2 (atm)`)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(24) +
        ggplot2::xlab("Time (yr)") +
        ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = ggplot2::element_line(lineend = 'square'),
              axis.text = ggplot2::element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank())
    
    # Plot for atmospheric carbon dioxide partial pressure
    } else if(var == "atm_pCO2"){
      plot <- ggplot2::ggplot(data = res.frame, ggplot2::aes(x = `% time (yr)` , y = `global pCO2 (atm)`)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(24) +
        ggplot2::xlab("Time (yr)") +
        ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = ggplot2::element_line(lineend = 'square'),
              axis.text = ggplot2::element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank())
    
    # Default plot for other variables
    } else {
      col_no <- 2  # Default to the second column for y-axis
      y_name <- names(res.frame)[col_no]  # Get the name of the y-axis variable
      plot <- ggplot2::ggplot(data = res.frame, ggplot2::aes(x = `% time (yr)` , y = get(names(res.frame)[col_no]))) +
        ggplot2::geom_line() +
        ggplot2::ylab(y_name) +
        ggplot2::xlab("Time (yr)") +
        ggplot2::theme_bw(24) +
        ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = ggplot2::element_line(lineend = 'square'),
              axis.text = ggplot2::element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank())
    }
  }
  
  # Return the generated plot
  return(plot)
}
