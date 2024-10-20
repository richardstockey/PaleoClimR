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
#' @examples
#' \dontrun{
#' plot <- cGENIE.res.plot(var = "ocn_temp", experiment = "experiment1")
#' print(plot)
#' }
#' 
#' @import ggplot2
#' @export
cGENIE.res.plot <- function(var, sub_var = "default", experiment){
  # Load the ggplot2 library for plotting
  library(ggplot2)
  
  # Import the data using the cGENIE.res.import function
  res.frame <- cGENIE.res.import(var = var, experiment = experiment)
  
  # Check if sub_var is set to "default"
  if(sub_var == "default"){
    # Plot for ocean temperature
    if(var == "ocn_temp"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `_surT (ice-free) (C)`)) +
        geom_line() +
        theme_bw(24) +
        xlab("Time (yr)") +
        theme(panel.border = element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    # Plot for ocean oxygen
    } else if(var == "ocn_O2"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global mean O2 (mol kg-1)`)) +
        geom_line() +
        theme_bw(24) +
        xlab("Time (yr)") +
        theme(panel.border = element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    # Plot for atmospheric oxygen partial pressure
    } else if(var == "atm_pO2"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global pO2 (atm)`)) +
        geom_line() +
        theme_bw(24) +
        xlab("Time (yr)") +
        theme(panel.border = element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    # Plot for atmospheric carbon dioxide partial pressure
    } else if(var == "atm_pCO2"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global pCO2 (atm)`)) +
        geom_line() +
        theme_bw(24) +
        xlab("Time (yr)") +
        theme(panel.border = element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    # Default plot for other variables
    } else {
      col_no <- 2  # Default to the second column for y-axis
      y_name <- names(res.frame)[col_no]  # Get the name of the y-axis variable
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = get(names(res.frame)[col_no]))) +
        geom_line() +
        ylab(y_name) +
        xlab("Time (yr)") +
        theme_bw(24) +
        theme(panel.border = element_rect(fill=NA, color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  }
  
  # Return the generated plot
  return(plot)
}
