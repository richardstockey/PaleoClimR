###################################################
# HADCM3.benthic.data.R
# Rich Stockey 20240324
# designed to extract data from imported .nc files (from e.g. from Valdes et al. 2021)
###################################################
# full comments to follow...

HADCM3.benthic.data <- function(var, file, experiment,
                       time.present = FALSE,
                       na.rm = FALSE
                       ){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default
  # palette_name currently has to be followed by (1000) or some other number
  # other options than parula would include - viridis and the many other options here https://r-charts.com/color-palettes/
  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)
  library(pals)
  library(viridis)

  df <- HADCM3.whole.ocean.data(var, file, experiment,
                                time.present = time.present,
                                na.rm = TRUE)

  # generate frame of just seafloor cells
  benth.levels <- df %>%
    group_by(lon.mid, lon.min, lon.max, lat.mid, lat.min, lat.max) %>%
    summarize(depth.level = max(depth.level, na.rm = TRUE))

  # now merge back with df to get values for those cells
  df.benth <- merge(df, benth.levels, all.y = TRUE)

return(df)
}

# testing...
# ggplot(df.benth, aes(y = lat.mid, x = lon.mid, color = var)) + geom_point()+
#   scale_colour_viridis_c()+theme_bw()

