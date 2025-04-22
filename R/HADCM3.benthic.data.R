#' Extract Benthic Data from NetCDF Files
#'
#' This function extracts benthic data from imported NetCDF (.nc) files, such as those from Valdes et al. (2021).
#'
#' @param var Character. The variable to extract from the NetCDF file.
#' @param file Character. The path to the NetCDF file.
#' @param experiment Character. The experiment identifier.
#' @param time.present Logical. If TRUE, extracts data for the present time. Default is FALSE.
#' @param na.rm Logical. If TRUE, removes NA values. Default is FALSE.
#'
#' @return A data frame containing the extracted benthic data.
#'
#' @details
#' This function reads in a NetCDF file and extracts data for benthic (seafloor) cells. It uses several R packages for data manipulation and visualization, including `RNetCDF`, `dplyr`, `sf`, `sp`, `ggspatial`, `reshape2`, `ggplot2`, `pals`, and `viridis`.
#'
#' The function first calls `HADCM3.whole.ocean.data` to get the full dataset, then filters this dataset to retain only the seafloor cells. The resulting data frame is returned.
#'
#' @note
#' Other projection options include Lambert Cylindrical Equal Area (EPSG: 6933). The `palette_name` parameter currently requires a numeric value (e.g., `parula(1000)`). Other color palette options include `viridis` and others available at \url{https://r-charts.com/color-palettes/}.
#'
#'
#' @import RNetCDF dplyr sf sp ggspatial reshape2 ggplot2 pals viridis
#' @export

HADCM3.benthic.data <- function(var, file, experiment,
       time.present = FALSE,
       na.rm = FALSE
       ){

                  # Call the function to get the full ocean dataset from the NetCDF file
                  df <- HADCM3.whole.ocean.data(var, file, experiment,
                                    time.present = time.present,
                                    na.rm = TRUE)

                  # Generate a data frame containing only the seafloor cells
                  # Group by longitude and latitude midpoints and calculate the maximum depth level for each group
                  benth.levels <- dplyr::group_by(df, lon.mid, lon.min, lon.max, lat.mid, lat.min, lat.max) %>%
                  dplyr::summarize(depth.level = max(depth.level, na.rm = TRUE))

                  # Merge the seafloor cells back with the original dataset to get the corresponding values
                  df.benth <- base::merge(df, benth.levels, all.y = TRUE)

                  # Return the data frame containing the benthic data
                  return(df.benth)
}

