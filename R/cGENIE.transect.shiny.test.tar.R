###################################################
# cGENIE.map.R
# Rich Stockey 20230922
# designed to make maps from imported .nc files (imported separately from cGENIE.nc.import.R because of hyperdimensionality) [?]
###################################################
# full comments to follow...

cGENIE.transect.shiny.test.tar <- function(file, var, name,
                       slice = 1,
                       dims = 3,
                       lat.or.lon = "lat",
                       year = "default",
                       unit.factor = 1,
                       min.value = 0,
                       max.value = 40,
                       intervals = 5,
                       continents.outlined,
                       scale.label = "[label goes here]",
                       model = "biogem",
                       palette_name = pals::parula(1000),
                       projection = 'ESRI:54012'){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default

  library(RNetCDF)
  library(dplyr)
  library(sf)
  library(sp)
  library(ggspatial)
  library(reshape2)
  library(ggplot2)
  library(stringr)
  # file <- "~/PETM.stuff/cgenie_archive/S22_056a_Tdep_remin_1PO4.tar.gz"

  file.name.short <- name %>% str_replace(".tar.gz", "")

  nc <- open.nc(archive::archive_extract(file, files = c(paste0(file.name.short, "/biogem/fields_biogem_3d.nc"))))

  # tar <- archive_read(file, "biogem/fields_biogem_3d.nc")
  #
  # nc <- open.nc(archive_read(file, "S22_056a_Tdep_remin_1PO4/biogem/fields_biogem_3d.nc"))
  #
  #
  # read.csv(archive_read(file, "S22_056a_Tdep_remin_1PO4/biogem/biogem_series_atm_temp.res"))

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees north
  lat.edges <- var.get.nc(nc, "lat_edges")
  lon <- var.get.nc(nc, "lon") # units: degrees east
  lon.edges <- var.get.nc(nc, "lon_edges")
  depth <- var.get.nc(nc, "zt") # units: metres
  depth.edges <- var.get.nc(nc, "zt_edges") # units: metres
  time <- var.get.nc(nc, "time") # units: year mid-point
  # note that not all of these general variables will be available for fields_biogem_2d (address later)

  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  if(year == "default"){
    time.step <- length(time)
  }

  # amend grid to project on 0 degs - note cGENIE differs from HADCM3
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges <= - 180] <- lon.edges[lon.edges <= - 180] + 360
    lon[lon <= - 180] <- lon[lon <= - 180] + 360
  }

  if(dims == 3){
    # generate dataframe of 2d genie slice from 3d genie array
    if(lat.or.lon == "lat"){
    df <- as.data.frame(cbind(
      rep(lat, times = length(depth), each = 1),
      rep(lat.edges[1:(length(lat.edges)-1)], times = length(depth), each = 1),
      rep(lat.edges[2:(length(lat.edges))], times = length(depth), each = 1),
      rep(depth, times = 1, each = length(lat)),
      rep(depth.edges[1:(length(depth.edges)-1)], times = 1, each = length(lat)),
      rep(depth.edges[2:(length(depth.edges))], times = 1, each = length(lat)),
      as.data.frame(melt(var.arr[slice,,, time.step]))$value))

    names(df) <- c("lat.mid",
                   "lat.min",
                   "lat.max",
                   "depth.mid",
                   "depth.min",
                   "depth.max",
                   "var"
    )
    }

    if(lat.or.lon == "lon"){
      df <- as.data.frame(cbind(
        rep(lon, times = length(depth), each = 1),
        rep(lon.edges[1:(length(lon.edges)-1)], times = length(depth), each = 1),
        rep(lon.edges[2:(length(lon.edges))], times = length(depth), each = 1),
        rep(depth, times = 1, each = length(lon)),
        rep(depth.edges[1:(length(depth.edges)-1)], times = 1, each = length(lon)),
        rep(depth.edges[2:(length(depth.edges))], times = 1, each = length(lon)),
        as.data.frame(melt(var.arr[slice,,, time.step]))$value))

      names(df) <- c("lon.mid",
                     "lon.min",
                     "lon.max",
                     "depth.mid",
                     "depth.min",
                     "depth.max",
                     "var"
      )
    }

  }
  # if(dims == 2){
  #   # generate dataframe of 2d genie slice from 3d genie array
  #   df <- as.data.frame(cbind(
  #     rep(lon, times = length(lat), each = 1),
  #     rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
  #     rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
  #     rep(lat, times = 1, each = length(lon)),
  #     rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
  #     rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
  #     as.data.frame(melt(var.arr[,, time.step]))$value))
  #
  #   names(df) <- c("lon.mid",
  #                  "lon.min",
  #                  "lon.max",
  #                  "lat.mid",
  #                  "lat.min",
  #                  "lat.max",
  #                  "var"
  #   )
  # }
  #
  # # eliminate cells outside of reasonable range
  # df <- df %>%
  #   filter(lon.max <= 180,
  #          lon.min >= -180,
  #          lat.max <= 90,
  #          lat.min >= -90
  #   )

  if(lat.or.lon == "lon"){ ## maybe keep this in from map making if doing lon-vert?

  # also update cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  df$lon.range <- abs(df$lon.min-df$lon.max)
  df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
  df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]
  }
  if(lat.or.lon == "lat"){

    map <- ggplot(data = df, aes(x = lat.mid, y = depth.mid, xmin = lat.min, xmax = lat.max, ymin = depth.min, ymax = depth.max, fill=var*unit.factor)) +
      geom_rect(color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
    scale_fill_stepsn(colours = palette_name,
                      #scale_fill_stepsn(colours = parula(1000),# seems like we can keep the n value (1000) just at something big?
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 12,
                                             barheight = 1,
                                             raster = FALSE,
                                             frame.colour = "grey6",
                                             frame.linewidth = 2/.pt,
                                             frame.linetype = 1,
                                             ticks = TRUE,
                                             ticks.colour = "grey6",
                                             ticks.linewidth = 2/.pt),
                      breaks = seq(min.value, max.value, intervals),
                      limits=c(min.value, max.value),
                      #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
    )+
    theme_bw()+
    scale_y_reverse()+
      theme(legend.position="bottom" ,
            plot.title = element_text(size=18),
            axis.title = element_text(size=18),
            axis.text = element_text(size=18)
      )+ coord_cartesian(expand = FALSE) +
      ylab("Depth (m)")+
      xlab("Latitude (°)")+
    labs(fill = scale.label, title = paste0(lon[slice], "° Longitude"))
  }

  if(lat.or.lon == "lon"){

    map <- ggplot(data = df, aes(x = lon.mid, y = depth.mid, xmin = lon.min, xmax = lon.max, ymin = depth.min, ymax = depth.max, fill=var*unit.factor)) +
      geom_rect(color = NA, linewidth=10, linetype=0) + # WGS 84 / Equal Earth Greenwich
      scale_fill_stepsn(colours = palette_name,
                        #scale_fill_stepsn(colours = parula(1000),# seems like we can keep the n value (1000) just at something big?
                        guide = guide_colorbar(title.position = "top",
                                               barwidth = 12,
                                               barheight = 1,
                                               raster = FALSE,
                                               frame.colour = "grey6",
                                               frame.linewidth = 2/.pt,
                                               frame.linetype = 1,
                                               ticks = TRUE,
                                               ticks.colour = "grey6",
                                               ticks.linewidth = 2/.pt),
                        breaks = seq(min.value, max.value, intervals),
                        limits=c(min.value, max.value),
                        #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
      )+
      theme_bw()+
      scale_y_reverse()+
      theme(legend.position="bottom" ,
            plot.title = element_text(size=18),
            axis.title = element_text(size=18),
            axis.text = element_text(size=18)
      )+ coord_cartesian(expand = FALSE) +
      ylab("Depth (m)")+
      xlab("Longitude (°)")+
      labs(fill = scale.label, title = paste0(lat[slice], "° Latitude"))
  }
  map
}


