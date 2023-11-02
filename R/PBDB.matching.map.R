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
                       family
                       ){

  library(readr)

  #Valdes_stage <- "380_0_MaBP"

  if(terrestrial.or.marine == "marine"){
    load("/Users/rgs1e22/Phanero_niches/marine_cleaned_binned.RData")
    occs <- marine_cleaned_binned
  }
  if(terrestrial.or.marine == "terrestrial"){
    load("/Users/rgs1e22/Phanero_niches/terrestrial_cleaned_binned.RData")
    occs <- terrestrial_cleaned_binned
  }

  stage_translations <- read_csv("~/Phanero_niches/stage_translations.csv")
  stage_translations <- filter(stage_translations, !is.na(stage_translations$diDyn_stage))

  divdyn_stage <- stage_translations$diDyn_stage[stage_translations$description == Valdes_stage]

  stage_occs <- filter(occs, stage == divdyn_stage)

  library(stringr)

  age_char <- Valdes_stage %>% str_replace("_MaBP", "")
  age_char2 <- age_char %>% str_replace("_", ".")
  age_num <- as.numeric(age_char2)

  # make all of the stage occurences the same age so that they match the valdes plate model (think more about but seems best)
  stage_occs$age <- age_num
  library(palaeoverse)

  stage_occs_rotd <- palaeorotate(occdf = stage_occs, lng="lng", lat="lat", age="age", model="PALEOMAP", method="point", uncertainty=FALSE, round=1 )

  rotd_coords <- cbind(stage_occs_rotd$p_lng, stage_occs_rotd$p_lat)
  rotd_coords <- na.omit(rotd_coords)

  rotd_coords_sp <- SpatialPoints(coords = rotd_coords)
  rotd_coords_spsf <- st_as_sf(rotd_coords_sp)
  st_crs(rotd_coords_spsf) = '+proj=longlat +ellps=sphere'

  HADCM3.map.w.fossils <- HADCM3.map +
    #geom_point(data = stage_occs_rotd, aes(x = p_lng, y = p_lat), shape = 21, size = 5, fill = "#D44D44")
  geom_sf(data = rotd_coords_spsf %>% st_transform(projection), aes(geometry = geometry), shape = 21, size = 4, alpha = 0.6, fill = "#D44D44") # WGS 84 / Equal Earth Greenwich

  HADCM3.map.w.fossils
}