###################################################
# divdyn_to_HADCM3.R
# Rich Stockey 20240115
# designed to convert DivDyn PBDB stages to HADCM3 stages used by Valdes et al. 2021
###################################################
# 20240201 - updated to not be dependent on csv table on RGS macbook pro

divdyn_to_HADCM3 <- function(divdyn_stage
    ){
  #divdyn_stage can be a vector or a single value (probably) but is expected to be a vector

  # likely you will be identifying a column in a dataframe as divdyn_stage = dataframe$column

  library(readr)
  library(stringr)
  library(dplyr)

  stage_no_spaces <- divdyn_stage %>% str_replace(" ", "_")

  # at some point want to make this an RData file!
  stage_translations <- load("stage_translations.RData")
  stage_translations <- filter(stage_translations, !is.na(stage_translations$diDyn_stage))

  Valdes_stage_age <- as.numeric()
  # surely there is a nice quick way to do this, but slopping loop for now!
  for(i in 1:length(stage_no_spaces)){
  Valdes_stage_age[i] <- stage_translations$description[stage_translations$diDyn_stage_no_spaces == stage_no_spaces[i]]
}
  return(Valdes_stage_age)
}
