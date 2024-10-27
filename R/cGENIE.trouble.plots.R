#' cGENIE.trouble.plots
#'
#' Generates key summary plots for cGENIE models on one A4-ish sheet of paper. The key plots include:
#' - Sea surface temperature through time
#' - Bathymetry
#' - Barotropic streamfunction
#' - Global marine O2 through time
#' - Surface alkalinity map (commented out)
#' - Surface H2S map (commented out)
#'
#' This function produces a 3x2 landscape paneled plot.
#'
#' @param experiment The experiment data to be used for generating the plots.
#' @param directory The directory where the plot will be saved. Default is the current working directory.
#' @param file.name The name of the file to save the plot as. Default is "cGENIE.trouble.plots".
#' @param format The format of the saved plot file. Default is "pdf".
#' @param save Logical indicating whether to save the plot. Default is FALSE.
#'
#' @return A ggplot object containing the arranged summary plots.
#' @export
#' @import ggplot2
#' @import gridExtra

cGENIE.trouble.plots <- function(experiment, directory="default", file.name = "cGENIE.trouble.plots", format = "pdf", save = FALSE){
  # Load required library
  library(egg)

  # Ensure the experiment data is assigned
  experiment <- experiment

  # Generate sea surface temperature plot through time
  temp.time <- cGENIE.res.plot(var = "ocn_temp",
                               experiment = experiment)

  # Generate bathymetry plot
  bathymetry <- cGENIE.map(var = "grid_topo",
                           experiment = experiment,
                           dims = 2,
                           unit.factor = -1,
                           min.value = -5000,
                           max.value = 0,
                           intervals = 500,
                           continents.outlined = FALSE,
                           scale.label = expression("Ocean Depth (m)"))

  # Generate barotropic streamfunction plot
  streamfunction <- cGENIE.overlay.map(var = "phys_psi",
                                       experiment = experiment,
                                       dims = 2,
                                       unit.factor = 1,
                                       min.value = -50,
                                       max.value = 50,
                                       intervals = 10,
                                       continents.outlined = FALSE,
                                       scale.label = "Barotropic streamfunction (Sv)")

  # Generate global marine O2 plot through time
  O2.time <- cGENIE.res.plot(var = "ocn_O2",
                             experiment = experiment)

  # Optional: Generate surface alkalinity map (commented out)
  # ALK.map.surface <- cGENIE.map(var = "ocn_sur_ALK",
  #                               experiment = experiment,
  #                               dims = 2,
  #                               unit.factor = 1,
  #                               min.value = 0,
  #                               max.value = 0.0065,
  #                               intervals = 0.00001,
  #                               continents.outlined = FALSE,
  #                               scale.label = expression("Alkalinity"))

  # Optional: Generate surface H2S map (commented out)
  # O2.map.surfish <- cGENIE.map(var = "ocn_O2",
  #                              experiment = experiment,
  #                              depth = 2,
  #                              unit.factor = 1e6,
  #                              min.value = 0,
  #                              max.value = 250,
  #                              intervals = 25,
  #                              continents.outlined = FALSE,
  #                              scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))

  # Set the directory to the current working directory if not specified
  if(directory == "default"){
    directory <- getwd()
  }

  # Arrange the generated plots into a 2x2 grid
  sum <- ggarrange(temp.time,
                   bathymetry,
                   O2.time,
                   streamfunction,
                   ncol = 2)

  # Save the plot to a file if save is TRUE
  if(save == TRUE){
    ggsave(file = paste0(directory, "/", file.name, ".", format), sum, height = 21, width = 29.7, units = "cm")
  }

  # Return the arranged plot
  return(sum)
}
