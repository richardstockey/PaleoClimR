###################################################
# cGENIE.res.import.R
# Rich Stockey 20230919
# designed to import .res files and make them normal data frames
###################################################
# full comments to follow...

cGENIE.res.import.tar <- function(var, file, name, model = "biogem"){
  library(readr)
  library(dplyr)
  library(stringr)
  setwd("~/PETM.stuff/cgenie_archive/")
  # name <- "S22_056a_Tdep_remin_1PO4.tar.gz"
  # var <- "ocn_O2"
  file.name.short <- name %>% str_replace(".tar.gz", "")



if(model == "biogem"){
  prefix <- "biogem_series_"
}

suffix <- ".res"

# import results table
res_file <- read_table(paste0(archive::archive_extract(file, files = c(paste0(file.name.short, "/", model, "/", prefix, var, suffix)))), show_col_types = FALSE)
# remove NAs (this import naturally creates a lot of columns just full of NAs)
res_file <- res_file %>% select_if(~ !any(is.na(.)))
# identify the column names from the res file. As these have a different delimiter we have to do this separately
res_file_names_frame <- read_delim(paste0(archive::archive_extract(file, files = c(paste0(file.name.short, "/", model, "/", prefix, var, suffix)))),
           delim = "/", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
# apply the column names we just obtained to the data columns in the res file
names(res_file) <- names(res_file_names_frame)
return(res_file)
}
