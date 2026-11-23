#' Calculate the thickness of oxygen minimum zones at each lat/lon grid cell in a cGENIE model
#'
#' For every lat/lon column, sums the thickness of all cells classified as oxygen minimum zone (OMZ)
#' based on the given threshold of oxygen concentration.
#'
#' @param experiment Directory containing the experiment's netCDF files.
#' @param OMZ.thresh A numeric value specifying the threshold oxygen concentration (in mol/kg, the
#'                units of the model output) at or below which a cell is classed as OMZ. Default is
#'                58.54e-6 (i.e. 58.54 umol/kg), based on Canfield 60uM
#'                https://enviromicro-journals.onlinelibrary.wiley.com/doi/10.1111/1462-2920.16192
#'                (just a conversion based on global mean density of 1.025 kg/L)
#' @param year Year to extract data for (default is "default", meaning the last time point).
#'
#' @return A dataframe with one row per lat/lon grid cell, containing the cell bounds plus
#'   \code{ocean.thickness} (total water depth, m), \code{omz.thickness} (summed OMZ thickness, m;
#'   NA for land columns) and \code{omz.fraction} (omz.thickness / ocean.thickness).
#' @export
#'

cGENIE.omz.thickness <- function(experiment,
                                 OMZ.thresh = 58.54e-6,
                                 year = "default") {
  
  omz.df <- cGENIE.omz(experiment = experiment,
                       OMZ.thresh = OMZ.thresh,
                       year = year)
  
  grid.cols <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max")
  
  omz.thickness <- omz.df %>%
    group_by(across(all_of(grid.cols))) %>%
    summarise(
      ocean.thickness = sum(.data$cell.thickness[!is.na(.data$OMZ.cell)]),
      omz.thickness   = sum(.data$cell.thickness[.data$omz.class %in% "OMZ"]),
      .groups = "drop"
    ) %>%
    mutate(
      omz.thickness = if_else(.data$ocean.thickness == 0, NA_real_, .data$omz.thickness),
      omz.fraction  = .data$omz.thickness / .data$ocean.thickness
    )
  
  return(omz.thickness)
}