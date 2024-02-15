###################################################
# PBDB.matching.map.R
# Rich Stockey 20231101
# designed to add PBDB occurences to a map produced by HADCM3.map
###################################################
# need to use the same projection!
# need to assign stage?
# filtering within
# full comments to follow...

# Valdes_stage corresponds to the all_dirs_short age of the stages used in Valdes 2021
# think more about best way to do this. Perhaps can just do some rounding and then set a linear slider scale??


PBDB.matching.map <- function(HADCM3.map,
                       Valdes_stage,
                       projection = 'ESRI:54012',
                       terrestrial.or.marine = "marine",
                       phylum,
                       order,
                       family,
                       genus,
                       pbdb.data,
                       data.already = TRUE,
                       var.present = FALSE,
                       var.name = "HADCM3.var"
                       ){

  library(readr)
  library(stringr)
  #Valdes_stage <- "525_0_MaBP"

  if(data.already == FALSE){
  if(terrestrial.or.marine == "marine"){
    load("/Users/rgs1e22/Phanero_niches/Phanerozoic_marine_cleaned_binned.RData")
    occs <- marine_cleaned_binned
  }
  if(terrestrial.or.marine == "terrestrial"){
    load("/Users/rgs1e22/Phanero_niches/Phanerozoic_terrestrial_cleaned_binned.RData")
    occs <- terrestrial_cleaned_binned
  }

  occs$stage_no_spaces <- occs$stage %>% str_replace(" ", "_")
  stage_translations <- read_csv("~/Phanero_niches/stage_translations.csv")
  stage_translations <- filter(stage_translations, !is.na(stage_translations$diDyn_stage))

  diDyn_stage_no_spaces <- stage_translations$diDyn_stage_no_spaces[stage_translations$description == Valdes_stage]

  stage_occs <- filter(occs, stage_no_spaces == diDyn_stage_no_spaces)

  library(stringr)

  age_char <- Valdes_stage %>% str_replace("_MaBP", "")
  age_char2 <- age_char %>% str_replace("_", ".")
  age_num <- as.numeric(age_char2)

  # make all of the stage occurences the same age so that they match the valdes plate model (think more about but seems best)
  stage_occs$age <- age_num
  library(palaeoverse)

  stage_occs_rotd <- palaeorotate(occdf = stage_occs, lng="lng", lat="lat", age="age", model="PALEOMAP", method="point", uncertainty=FALSE, round=1 )
  }
  if(data.already == TRUE){
    stage_occs_rotd <- pbdb.data
  }
  if(var.present == FALSE){
  rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat)
  rotd_coords <- na.omit(rotd_coords)
  rotd_coords_sp <- SpatialPoints(coords = rotd_coords)
  rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
  st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'
  HADCM3.map.w.fossils <- HADCM3.map +
    #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
    geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry), shape = 21, size = 4, alpha = 0.6, fill = "#D44D44") # WGS 84 / Equal Earth Greenwich

  }
  if(var.present == TRUE){
  # rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, get(paste0("stage_occs_rotd$", var.name)))
  rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, stage_occs_rotd$HADCM3.var)
  rotd_coords <- na.omit(rotd_coords)
  rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
  names(rotd_coords_sp) <- "HADCM3.var"

  # gardcoding in palettes for now  - 20231127
  palette_name_ocean <- pals::parula(1000)
  min.value_1 <- 0
  max.value_1 <- 40
  intervals_1 <- 5

  rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
  st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'
  HADCM3.map.w.fossils <- HADCM3.map +
    #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
    new_scale_fill() +
    scale_fill_stepsn(colours = palette_name_ocean,
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
                      breaks = seq(min.value_1, max.value_1, intervals_1),
                      limits=c(min.value_1, max.value_1),
                      #labels = c("0", "", "50", "", "100", "", "150", "", "200", "", "250")
    )+
    theme(legend.position="bottom")+
    labs(fill = "Fossil Occurrence Temperature (°C)")+
    geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = HADCM3.var), shape = 21, size = 4, alpha = 0.6) # WGS 84 / Equal Earth Greenwich
}
  if(var.present == "reefs"){
    stage_occs_rotd$REE_classification <- factor(stage_occs_rotd$REE_classification, levels=c('Archaeocyathids', #reds
                                                                              'Glass _sponges', #reds
                                                                              'Stromatoporoids', #reds
                                                                              'Rudist_bivalves', #purples
                                                                              'Hydrozoans', #oranges
                                                                              'Tube_worms', #greens
                                                                              'Rugose_corals',#blues
                                                                              'Tabulate_corals', #blues
                                                                              'Stony_corals')) #blues

    reef_builder.colors <- c('#6e1423',
                             '#b21e35',
                             '#e01e37',
                             '#b084cc',
                             'goldenrod2',
                             '#548c2f',
                             '#01497c',
                             '#2c7da0',
                             '#a9d6e5')

    rotd_coords <- as.data.frame(cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, paste(stage_occs_rotd$REE_classification)))

    rotd_coords$V1 <- as.numeric(rotd_coords$V1)
    rotd_coords$V2 <- as.numeric(rotd_coords$V2)

    rotd_coords$V3 <- factor(rotd_coords$V3, levels=c('Archaeocyathids', #reds
                                                                                              'Glass_sponges', #reds
                                                                                              'Stromatoporoids', #reds
                                                                                              'Rudist_bivalves', #purples
                                                                                              'Hydrozoans', #oranges
                                                                                              'Tube_worms', #greens
                                                                                              'Rugose_corals',#blues
                                                                                              'Tabulate_corals', #blues
                                                                                              'Stony_corals')) #blues
    rotd_coords <- na.omit(rotd_coords)
    rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
    names(rotd_coords_sp) <- "REE_classification"
    rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
    st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'

    REE_classes <- rotd_coords %>%
      group_by(V3) %>%
      tally() %>%
      tally() %>%
      as.numeric()


    HADCM3.map.w.fossils <- HADCM3.map +
      #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
      new_scale_fill() +
      scale_fill_manual(values=reef_builder.colors) +
      theme(legend.position="bottom")+
      guides(fill = guide_legend(nrow = REE_classes))+
      labs(fill = "REE classification")+
      geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = REE_classification), colour='black', shape=21,  size = 4, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

  }


  HADCM3.map.w.fossils
}
