###################################################
# cGENIE.sum.plots.R
# Rich Stockey 20230922
# designed to generate key summary plots for cGENIE models (- on one A4-ish sheet of paper!)
###################################################
# Key plots include
# - sea surface temperature through time
# - bathymetry
# - barotropic streamfunction
# - global marine O2 through time
# - surface alkalinity map
# - surface H2S map
# This produces a 3x2 landscape paneled plot
###################################################
# full comments to follow...

cGENIE.trouble.plots <- function(experiment, directory="default", file.name = "cGENIE.trouble.plots", format = "pdf", save = FALSE){
library(egg)

experiment <- experiment

temp.time <- cGENIE.res.plot(var = "ocn_temp",
                  experiment = experiment
  )

bathymetry <- cGENIE.map(var = "grid_topo",
                           experiment = experiment,
                           dims = 2,
                           # depth = 1,
                            unit.factor=-1,
                           min.value = -5000,
                           max.value = 0,
                           intervals = 500,
                           continents.outlined = FALSE,
                           scale.label = expression("Ocean Depth (m)"))

streamfunction <- cGENIE.overlay.map(var = "phys_psi",
                             experiment = experiment,
                             dims = 2,
                             unit.factor = 1,
                             min.value = -50,
                             max.value = 50,
                             intervals = 10,
                             continents.outlined = FALSE,
                             scale.label = "Barotropic streamfunction (Sv)")

O2.time <- cGENIE.res.plot(var = "ocn_O2",
                           experiment = experiment
)

# ALK.map.surface <- cGENIE.map(var = "ocn_sur_ALK",
#                             experiment = experiment,
#                             dims = 2,
#                             unit.factor=1,
#                             min.value = 0,
#                             max.value = 0.0065,
#                             intervals = 0.00001,
#                             continents.outlined = FALSE,
#                             scale.label = expression("Alkalinity"))

# O2.map.surfish <- cGENIE.map(var = "ocn_O2",
#                              experiment = experiment,
#                              depth = 2,
#                              unit.factor=1e6,
#                              min.value = 0,
#                              max.value = 250,
#                              intervals = 25,
#                              continents.outlined = FALSE,
#                              scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))


if(directory == "default"){
  directory <- getwd()
}

sum <- ggarrange(temp.time,
                 bathymetry,
                 O2.time,
                 streamfunction,
                 ncol = 2
)

if(save == TRUE){
ggsave(file = paste0(directory, file.name, ".", format), sum, height=21, width=28, units = "cm")
}

return(sum)
}
