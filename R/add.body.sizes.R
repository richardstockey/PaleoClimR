
add.body.sizes <- function(terrestrial.or.marine = "marine",
                            phylum,
                            order,
                            family
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

  occs$stage_no_spaces <- occs$stage %>% str_replace(" ", "_")
  stage_translations <- read_csv("~/Phanero_niches/stage_translations.csv")
  stage_translations <- filter(stage_translations, !is.na(stage_translations$diDyn_stage))

  diDyn_stage_no_spaces <- stage_translations$diDyn_stage_no_spaces[stage_translations$description == Valdes_stage]

  stage_occs <- filter(occs, stage_no_spaces == diDyn_stage_no_spaces)

  sizeData <- read_delim("~/Heim+_2020_data/sizeData.txt",
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

  stage_occs_merged <- merge(occs, sizeData, by.x = "genus", by.y = "taxon_genus"
  )

}
