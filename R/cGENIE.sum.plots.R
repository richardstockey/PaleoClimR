###################################################
# cGENIE.sum.plots.R
# Rich Stockey 20230922
# designed to generate key summary plots for cGENIE models (- on one A4-ish sheet of paper!)
###################################################
# Key plots include
# - sea surface temperature through time
# - sea surface temperature map
# - bottom water temperature map
# - global marine O2 through time
# - second level marine O2 map
# - bottom water  O2 map
# This produces a 3x2 landscape panelled plot
###################################################
# full comments to follow...

cGENIE.sum.plots <- function(experiment, directory="default", file.name = "cGENIE.sum.plots", format = "pdf", save = FALSE){
library(egg)

experiment <- experiment

temp.time <- cGENIE.res.plot(var = "ocn_temp",
                  experiment = experiment
  )

temp.map.surf <- cGENIE.map(var = "ocn_temp",
                           experiment = experiment,
                           depth = 1,
                           unit.factor=1,
                           min.value = 0,
                           max.value = 35,
                           intervals = 5,
                           continents.outlined = FALSE,
                           scale.label = expression("Seawater Temperature (°)"))

temp.map.benth <- cGENIE.map(var = "ocn_ben_temp",
                             experiment = experiment,
                             dims = 2,
                             unit.factor=1,
                             min.value = 0,
                             max.value = 40,
                             intervals = 5,
                             continents.outlined = FALSE,
                             scale.label = expression("Seawater Temperature (°)"))

O2.time <- cGENIE.res.plot(var = "ocn_O2",
                           experiment = experiment
)

O2.map.surfish <- cGENIE.map(var = "ocn_O2",
                            experiment = experiment,
                            depth = 2,
                            unit.factor=1e6,
                            min.value = 0,
                            max.value = 250,
                            intervals = 25,
                            continents.outlined = FALSE,
                            scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))

O2.map.benth <- cGENIE.map(var = "ocn_ben_O2",
                           experiment = experiment,
                           dims = 2,
                           unit.factor=1e6,
                           min.value = 0,
                           max.value = 250,
                           intervals = 25,
                           continents.outlined = FALSE,
                           scale.label = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))


if(directory == "default"){
  directory <- getwd()
}

sum <- ggarrange(temp.time,
                 temp.map.surf,
                 temp.map.benth,
                 O2.time,
                 O2.map.surfish,
                 O2.map.benth,
                 ncol = 3
)

if(save == TRUE){
ggsave(file = paste0(directory, file.name, ".", format), sum, height=14.5, width=25, units = "cm")
}

return(sum)
}
