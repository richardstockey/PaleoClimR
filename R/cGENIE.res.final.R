#' cGENIE.res.final
#'
#' This function imports .res files and converts them into data frames, then extracts the final data point.
#'
#' @param var A character string specifying the variable to be imported. Options include "ocn_temp" and "ocn_O2".
#' @param sub_var A character string specifying the sub-variable to be used. Default is "default".
#' @param experiment A character string specifying the experiment from which the data is to be imported.
#'
#' @return The final data point of the specified variable from the imported .res file.
#'
#' @details
#' The function first imports the .res file corresponding to the specified variable and experiment using the `cGENIE.res.import` function.
#' It then extracts the final data point of the specified variable. If `sub_var` is set to "default", the function uses predefined sub-variables
#' for "ocn_temp" and "ocn_O2". For other variables, it extracts the value from the second column of the final row.
#'
#' @examples
#' \dontrun{
#' final_temp <- cGENIE.res.final(var = "ocn_temp", experiment = "experiment1")
#' final_O2 <- cGENIE.res.final(var = "ocn_O2", experiment = "experiment2")
#' }
#'
#' @export
cGENIE.res.final <- function(var, sub_var = "default", experiment) {
  res.frame <- cGENIE.res.import(var = var, experiment = experiment)

  # Check if sub_var is set to "default"
  if (sub_var == "default") {
    # If var is "ocn_temp", extract the final value of the surface temperature (ice-free) in Celsius
    if (var == "ocn_temp") {
      val <- res.frame$`_surT (ice-free) (C)`[nrow(res.frame)]
    # If var is "ocn_O2", extract the final value of the global mean O2 in mol/kg
    } else if (var == "ocn_O2") {
      val <- res.frame$`global mean O2 (mol kg-1)`[nrow(res.frame)]
    # For other variables, extract the value from the second column of the final row
    } else {
      val <- res.frame[nrow(res.frame), 2]
    }
  }
  return(val)
}
