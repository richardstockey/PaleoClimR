#' Categorise ocean cells based on whether they are in an oxygen minimum zone
#'
#' This function returns a cGENIE.data dataframe including a column where cells are classified as
#' oxygen minimum zones (OMZs) or oxygenated based on the given threshold of oxygen concentration.
#' The calculation is performed using data from a specified experiment in NetCDF format.
#'
#' @param experiment Directory containing the experiment's netCDF files.
#' @param OMZ.thresh A numeric value specifying the threshold oxygen concentration (in mol/kg, the
#'                units of the model output) at or below which a cell is classed as OMZ. Default is
#'                58.54e-6 (i.e. 58.54 umol/kg), based on Canfield 60uM
#'                https://enviromicro-journals.onlinelibrary.wiley.com/doi/10.1111/1462-2920.16192
#'                (just a conversion based on global mean density of 1.025 kg/L)
#' @param year Year to extract data for (default is "default", meaning the last time point).
#'
#' @return A cGENIE.data dataframe (one row per cell) with additional columns:
#'   \code{cell.thickness} (m), \code{OMZ.cell} ("OMZ" or "oxygenated", NA for land / no data)
#'   and \code{omz.class} (the same classification as a factor).
#' @export
#'

cGENIE.omz <- function(experiment,
                       OMZ.thresh = 58.54e-6,
                       year = "default") {
  
  O2 <- cGENIE.data.3D(experiment = experiment,
                       var = "ocn_O2",
                       year = year)
  
  # all cells are the same area in cGENIE, so only thickness is needed for proportional volume
  O2$cell.thickness <- O2$depth.max - O2$depth.min
  
  # classify each cell
  OMZ.categorise <- O2 %>%
    mutate(OMZ.cell = case_when(
      .data$var <= OMZ.thresh ~ "OMZ",
      .data$var >  OMZ.thresh ~ "oxygenated"
    )) %>%
    mutate(omz.class = factor(.data$OMZ.cell,
                              levels = c("OMZ", "oxygenated")))
  
  return(OMZ.categorise)
}