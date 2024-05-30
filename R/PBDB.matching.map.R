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
                       phylum = NA,
                       class = NA,
                       order = NA,
                       family = NA,
                       genus = NA,
                       pbdb.data,
                       data.already = TRUE,
                       var.present = FALSE,
                       var.name = "HADCM3.var"
                       ){

  library(readr)
  library(stringr)
  library(ggnewscale)
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

  if(is.na(phylum) == FALSE){
    filter(occs, phylum == phylum)
  }
    if(is.na(class) == FALSE){
      filter(occs, class == class)
    }
    if(is.na(order) == FALSE){
      filter(occs, order == order)
    }
    if(is.na(family) == FALSE){
      filter(occs, family == family)
    }
    if(is.na(genus) == FALSE){
      filter(occs, genus == genus)

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

    if(is.na(phylum) == FALSE){
      filter(stage_occs_rotd, phylum == phylum)
    }
    if(is.na(class) == FALSE){
      filter(stage_occs_rotd, class == class)
    }
    if(is.na(order) == FALSE){
      filter(stage_occs_rotd, order == order)
    }
    if(is.na(family) == FALSE){
      filter(stage_occs_rotd, family == family)
    }
    if(is.na(genus) == FALSE){
      filter(stage_occs_rotd, genus == genus)

    }

  }
  if(var.present == FALSE){
  rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat)
  rotd_coords <- na.omit(rotd_coords)
  rotd_coords_sp <- SpatialPoints(coords = rotd_coords)
  rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
  st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'
  HADCM3.map.w.fossils <- HADCM3.map +
    #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
    geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry), shape = 21, size = 6, stroke = 1.0, alpha = 0.6, fill = "#D44D44") # WGS 84 / Equal Earth Greenwich

  }
  if(var.present == TRUE){

    if(var.name == "HADCM3.var"){
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
    geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = HADCM3.var), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich
    }
    if(var.name == "ocn_O2"){
      # rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, get(paste0("stage_occs_rotd$", var.name)))
      rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, stage_occs_rotd$HADCM3.var*10^6)
      rotd_coords <- na.omit(rotd_coords)
      rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
      names(rotd_coords_sp) <- "ocn_O2"

      # gardcoding in palettes for now  - 20231127
      palette_name_ocean <- pals::parula(1000)
      min.value_1 <- 0
      max.value_1 <- 250
      intervals_1 <- 25

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
        labs(fill = expression("Dissolved O"[2]*" ("*mu*"mol/kg)"))+
        geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = ocn_O2), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich
    }
    if(var.name == "ocn_temp"){
      # rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, get(paste0("stage_occs_rotd$", var.name)))
      rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, stage_occs_rotd$HADCM3.var)
      rotd_coords <- na.omit(rotd_coords)
      rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
      names(rotd_coords_sp) <- "ocn_temp"

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
        labs(fill = expression("Temperature (°C)"))+
        geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = ocn_temp), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich
    }
    if(var.name == "misc_pH"){
      # rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, get(paste0("stage_occs_rotd$", var.name)))
      rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat, stage_occs_rotd$HADCM3.var)
      rotd_coords <- na.omit(rotd_coords)
      rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
      names(rotd_coords_sp) <- "ocn_O2"

      # gardcoding in palettes for now  - 20231127
      palette_name_ocean <- pals::parula(1000)
      min.value_1 <- 6
      max.value_1 <- 8
      intervals_1 <- .25

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
        labs(fill = expression("pH"))+
        geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = misc_ph), shape = 21, size = 6, stroke = 1.0, alpha = 0.6) # WGS 84 / Equal Earth Greenwich
    }
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


    rotd_coords <- as.data.frame(cbind(stage_occs_rotd$p_lng,
                                       stage_occs_rotd$p_lat,
                                       paste(stage_occs_rotd$REE_classification)))

   REE_classification <- c('Archaeocyathids', #reds
      'Glass _sponges', #reds
      'Stromatoporoids', #reds
      'Rudist_bivalves', #purples
      'Hydrozoans', #oranges
      'Tube_worms', #greens
      'Rugose_corals',#blues
      'Tabulate_corals', #blues
      'Stony_corals')
   p_lng <- rep(NA, length(REE_classification))
   p_lat <- rep(NA, length(REE_classification))

   placeholders <- as.data.frame(cbind(p_lng, p_lat, REE_classification))

   names(rotd_coords) <- names(placeholders)

   rotd_coords <- rbind(rotd_coords, placeholders)

    rotd_coords$p_lng <- as.numeric(rotd_coords$p_lng)
    rotd_coords$p_lat <- as.numeric(rotd_coords$p_lat)

    rotd_coords$REE_classification <- factor(rotd_coords$REE_classification, levels=c('Archaeocyathids', #reds
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

    REE_sum <- rotd_coords %>%
      group_by(REE_classification) %>%
      tally()

    REE_sum$reef_builder.color <- NA

    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Archaeocyathids'] <- '#6e1423'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Glass_sponges'] <- '#b21e35'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Stromatoporoids'] <- '#e01e37'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Rudist_bivalves'] <- '#b084cc'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Hydrozoans'] <- 'goldenrod2'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Tube_worms'] <- '#548c2f'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Rugose_corals'] <- '#01497c'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Tabulate_corals'] <- '#2c7da0'
    REE_sum$reef_builder.color[REE_sum$REE_classification == 'Stony_corals'] <- '#a9d6e5'

    reef_builder.colors <- REE_sum$reef_builder.color

    REE_classes <- rotd_coords %>%
      group_by(REE_classification) %>%
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
  if(var.present == "reefs2"){
    stage_occs_rotd$REE_classification <- factor(stage_occs_rotd$REE_classification, levels=c('Archaeocyathids', #reds
                                                                                              'Glass_sponges', #reds
                                                                                              'Stromatoporoids', #reds
                                                                                              'Rudist_bivalves', #purples
                                                                                              'Hydrozoans', #oranges
                                                                                              'Tube_worms', #greens
                                                                                              'Rugose_corals',#blues
                                                                                              'Tabulate_corals', #blues
                                                                                              'Stony_corals')) #blues
    stage_occs_rotd$reef_builder.color <- NA

    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Archaeocyathids'] <- '#6e1423'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Glass_sponges'] <- '#b21e35'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Stromatoporoids'] <- '#e01e37'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Rudist_bivalves'] <- '#b084cc'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Hydrozoans'] <- 'goldenrod2'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Tube_worms'] <- '#548c2f'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Rugose_corals'] <- '#01497c'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Tabulate_corals'] <- '#2c7da0'
    stage_occs_rotd$reef_builder.color[stage_occs_rotd$REE_classification == 'Stony_corals'] <- '#a9d6e5'

    rotd_coords <- as.data.frame(cbind(stage_occs_rotd$p_lng,
                                       stage_occs_rotd$p_lat,
                                       paste(stage_occs_rotd$reef_builder.color)))

    names(rotd_coords) <- c('p_lng', 'p_lat', 'reef_builder.color')


    rotd_coords$p_lng <- as.numeric(rotd_coords$p_lng)
    rotd_coords$p_lat <- as.numeric(rotd_coords$p_lat)

    rotd_coords <- na.omit(rotd_coords)
    rotd_coords_sp <- SpatialPointsDataFrame(coords = rotd_coords[,1:2], data = as.data.frame(rotd_coords[,3]))
    names(rotd_coords_sp) <- "reef_builder.color"
    rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
    st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'

    colours <- rotd_coords %>%
      group_by(reef_builder.color) %>%
      tally() %>%
      tally() %>%
      as.numeric()


    HADCM3.map.w.fossils <- HADCM3.map +
      #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
      new_scale_fill() +
      scale_fill_identity() +
      theme(legend.position="bottom")+
      guides(fill = guide_legend(nrow = colours))+
      labs(fill = "Reef Builder Colour")+
      geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry, fill = reef_builder.color), colour='black', shape=21,  size = 4, alpha = 0.6) # WGS 84 / Equal Earth Greenwich

  }

  HADCM3.map.w.fossils
}
