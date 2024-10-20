#' Generate Key Summary Plots for cGENIE Models
#'
#' This function generates a set of key summary plots for cGENIE models,
#' designed to fit on one A4-ish sheet of paper. The plots include:
#' - Sea surface temperature through time
#' - Sea surface temperature map
#' - Bottom water temperature map
#' - Global marine O2 through time
#' - Second level marine O2 map
#' - Bottom water O2 map
#'
#' The function produces a 3x2 landscape paneled plot.
#'
#' @param experiment A character string specifying the experiment name.
#' @param directory A character string specifying the directory to save the plot. Default is the current working directory.
#' @param file.name A character string specifying the name of the output file. Default is "cGENIE.sum.plots".
#' @param format A character string specifying the format of the output file. Default is "pdf".
#' @param save A logical value indicating whether to save the plot. Default is FALSE.
#'
#' @return A ggplot object containing the arranged summary plots.
#' @import egg
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' cGENIE.sum.plots(experiment = "experiment_name", save = TRUE)
#' }
#'
cGENIE.sum.plots <- function(experiment = NULL, directory="default", file.name = "cGENIE.sum.plots", format = "pdf", save = FALSE){
  # Load necessary library
  library(egg)
  library(ggplot2)

  # Generate a plot for sea surface temperature through time
  temp.time <- cGENIE.res.plot(var = "ocn_temp",
                               experiment = experiment
  )

  # Generate a map for sea surface temperature at the surface level
  temp.map.surf <- cGENIE.map(var = "ocn_temp",
                              experiment = experiment,
                              depth = 1,
                              unit.factor=1,
                              min.value = 0,
                              max.value = 35,
                              intervals = 5,
                              continents.outlined = FALSE,
                              scale.label = expression("Seawater Temperature (°C)"))

# Generate a map for bottom water temperature
temp.map.benth <- cGENIE.map(var = "ocn_ben_temp",
               experiment = experiment,
               dims = 2,
               unit.factor=1,
               min.value = 0,
               max.value = 40,
               intervals = 5,
               continents.outlined = FALSE,
               scale.label = expression("Seawater Temperature (°C)"))

# Generate a plot for global marine O2 through time
O2.time <- cGENIE.res.plot(var = "ocn_O2",
               experiment = experiment
)

# Generate a map for dissolved O2 at the second level (near surface)
O2.map.surfish <- cGENIE.map(var = "ocn_O2",
              experiment = experiment,
              depth = 2,
              unit.factor=1e6,
              min.value = 0,
              max.value = 250,
              intervals = 25,
              continents.outlined = FALSE,
              scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))

# Generate a map for bottom water dissolved O2
O2.map.benth <- cGENIE.map(var = "ocn_ben_O2",
               experiment = experiment,
               dims = 2,
               unit.factor=1e6,
               min.value = 0,
               max.value = 250,
               intervals = 25,
               continents.outlined = FALSE,
               scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))

# Check if the directory is set to default, if so, use the current working directory
if(directory == "default"){
  directory <- getwd()
}

# Arrange all the generated plots into a 3x2 grid
sum <- ggarrange(temp.time,
         temp.map.surf,
         temp.map.benth,
         O2.time,
         O2.map.surfish,
         O2.map.benth,
         ncol = 3
)

# Save the arranged plot if the save parameter is TRUE
if(save == TRUE){
  ggsave(file = paste0(directory, file.name, ".", format), sum, height=14.5, width=25, units = "cm")
}

# Return the arranged plot
return(sum)
}
