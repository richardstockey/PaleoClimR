###################################################
# PBDB.matching.data.R
# Rich Stockey 20231102
# designed to extract PBDB data corresponding to data generated from HADCM3.data
###################################################
# need to use the same projection!
# need to assign stage?
# filtering within
# full comments to follow...

# Valdes_stage corresponds to the all_dirs_short age of the stages used in Valdes 2021
# think more about best way to do this. Perhaps can just do some rounding and then set a linear slider scale??

# this works with cgenie, just gotta pass cgenie data to the function...
PBDB.matching.data <- function(HADCM3.data,
                       Valdes_stage,
                       terrestrial.or.marine = "marine",
                       phylum = NA,
                       class = NA,
                       order = NA,
                       family = NA,
                       genus = NA,
                       add.body.sizes = TRUE
                       ){

  library(readr)
  library(stringr)
  #Valdes_stage <- "525_0_MaBP"

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

  if(add.body.sizes == TRUE){
  sizeDataOld <- read_delim("~/Heim+_2020_data/sizeData.txt",
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

  sizeData <- read_delim("~/Monarrez+_2024_Body_Size/Phanerozoic_body_size_data.txt",
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)
  sizeData <- sizeData %>%
    filter(!is.na(phylum))

  stage_occs <- merge(stage_occs, sizeData, by.x = "genus", by.y = "taxon_name")
  }

  library(stringr)

  age_char <- Valdes_stage %>% str_replace("_MaBP", "")
  age_char2 <- age_char %>% str_replace("_", ".")
  age_num <- as.numeric(age_char2)

  # make all of the stage occurences the same age so that they match the valdes plate model (think more about but seems best)
  stage_occs$age <- age_num
  library(palaeoverse)

  stage_occs_rotd <- palaeorotate(occdf = stage_occs, lng="lng", lat="lat", age="age", model="PALEOMAP", method="point", uncertainty=FALSE, round=1 )

  return(stage_occs_rotd)

}
