#' Calculate the global volume of oxygen minimum zones in a given cGENIE model
#'
#' This function calculates the proportion of the ocean volume that is classified as oxygen minimum
#' zones (OMZs) based on the given threshold of oxygen concentration. The calculation is performed
#' using data from a specified experiment in NetCDF format. Cells are weighted by thickness, which is
#' proportional to volume because all cGENIE cells have the same area.
#'
#' @param experiment Directory containing the experiment's netCDF files.
#' @param OMZ.thresh A numeric value specifying the threshold oxygen concentration (in mol/kg, the
#'                units of the model output) at or below which a cell is classed as OMZ. Default is
#'                58.54e-6 (i.e. 58.54 umol/kg), based on Canfield 60uM
#'                https://enviromicro-journals.onlinelibrary.wiley.com/doi/10.1111/1462-2920.16192
#'                (just a conversion based on global mean density of 1.025 kg/L)
#' @param year Year to extract data for (default is "default", meaning the last time point).
#'
#' @return A numeric value representing the proportion of the ocean volume that is classified as an
#'   OMZ (between 0 and 1).
#' @export
#'

cGENIE.omz.volume <- function(experiment,
                              OMZ.thresh = 58.54e-6,
                              year = "default") {
  
  omz.df <- cGENIE.omz(experiment = experiment,
                       OMZ.thresh = OMZ.thresh,
                       year = year)
  
  # thickness-weighted fraction of ocean volume in each class
  omz.sum <- omz.df %>%
    filter(!is.na(.data$OMZ.cell)) %>%                                   # drop land / NA cells
    count(.data$omz.class, wt = .data$cell.thickness, .drop = FALSE) %>% # sum thickness, not cell count
    mutate(vol = .data$n / sum(.data$n))
  
  # proportion of total ocean volume that is OMZ (between 0 and 1)
  omz.volume <- omz.sum$vol[omz.sum$omz.class == "OMZ"]
  
  return(omz.volume)
}