#' Summarise Global Depth Averages and Standard Deviations from cGENIE NetCDF Data
#'
#' This function extracts and processes a 3D variable from a cGENIE NetCDF file.
#' It computes the mean and standard deviation for the variable at different depth levels,
#' grouped by depth ranges.
#'
#' @param var A character string specifying the variable name to extract from the NetCDF file (e.g., "ocn_O2").
#' @param experiment A character string indicating the path to the cGENIE experiment directory.
#' @param year A character string or numeric value specifying the year to extract. Default is "default" which selects the last time step.
#' @param model A character string indicating the model type (default is "biogem").
#' @return A dataframe with summarized mean and standard deviation of the variable grouped by depth ranges.
#' @details The function reads the specified 3D variable from the NetCDF file, groups the data by depth, and calculates summary statistics (mean and standard deviation). The depth is grouped by minimum and maximum depth edges.
#' @import dplyr
#' @import RNetCDF
#' @import tidyr
#' @export

cGENIE.depth.sum <- function(var, experiment, year = "default", model = "biogem") {
  # Call helper function to extract 3D data from the NetCDF file (assuming `cGENIE.data.3D` exists)
  var.3D.df <- cGENIE.data.3D(experiment = experiment, var = var, year = year)

  # Group the data by depth, and summarize mean and standard deviation for the specified variable
  var.depth.sum <- var.3D.df %>%
    dplyr::group_by(depth, depth.min, depth.max) %>%
    dplyr::summarise(mean.var = mean(var, na.rm = TRUE),  # Mean of the variable at each depth level
                     sd.var = stats::sd(var, na.rm = TRUE)) %>%  # Standard deviation at each depth level
    tidyr::drop_na()  # Remove any rows with NA values

  # Return the summarized data as a dataframe
  return(var.depth.sum)
}
