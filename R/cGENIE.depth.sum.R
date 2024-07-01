###################################################
# cGENIE.depth.sum.R
# Rich Stockey 20240120
# designed to extract all data from imported cGENIE .nc files
# and then summarise global depth averages and standard deviations.
###################################################
# full comments to follow...

cGENIE.depth.sum <- function(var, experiment,
                        year = "default",
                        model = "biogem"){


  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)
  library(pals)
  library(viridis)

  var.3D.df <- cGENIE.data.3D(experiment = exp,
                          var = var, year = year

  )

  var.depth.sum <- var.3D.df %>%
    group_by(depth, depth.min, depth.max) %>%
    summarise(mean.var = mean(var, na.rm=T),  sd.var = sd(var, na.rm=T)) %>%
    na.omit()

return(var.depth.sum)
}


