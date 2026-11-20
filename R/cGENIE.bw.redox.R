#' Summarize Bottom-Water Redox Conditions
#'
#' This function processes output from a cGENIE experiment to summarize the redox conditions
#' of bottom-water environments. It categorizes the ocean bottom water into anoxic, suboxic, and oxic conditions
#' based on specified oxygen thresholds, and returns the fraction of ocean grid cells in each class.
#'
#' @param experiment A character string specifying the path to the experiment folder, where the necessary NetCDF files are stored.
#' @param anox.thresh Numeric. The oxygen threshold (in mol/kg) for classifying anoxic bottom waters. Default is 0.
#' @param subox.thresh Numeric. The oxygen threshold (in mol/kg) for classifying suboxic bottom waters. Default is 4.8e-6 (based on Sperling et al. 2015).
#' @param lower.lims Logical. If TRUE, sets a minimum seafloor fraction of 0.01% for all sinks, for logarithmic plotting purposes.
#' The excess is removed from the largest sink so that the fractions still sum to 1. Default is TRUE.
#'
#' @details This function extracts the bottom-water oxygen field (`ocn_ben_O2`) from a cGENIE experiment using
#' \code{\link{cGENIE.data}} and categorizes each ocean grid cell as anoxic, suboxic, or oxic. Land cells (NA) are excluded.
#' Fractions are based on grid-cell counts, which are equivalent to area fractions on the equal-area cGENIE grid.
#'
#' The redox classification thresholds are based on oxygen concentration values, with:
#' - Anoxic: oxygen concentration \eqn{\leq} anox.thresh
#' - Suboxic: oxygen concentration \eqn{>} anox.thresh and \eqn{\leq} subox.thresh
#' - Oxic: oxygen concentration \eqn{>} subox.thresh
#'
#' @return A list with the following components (summing to 1):
#' \describe{
#'   \item{f.anox}{Fraction of bottom-water grid cells classified as anoxic.}
#'   \item{f.subox}{Fraction of bottom-water grid cells classified as suboxic.}
#'   \item{f.ox}{Fraction of bottom-water grid cells classified as oxic.}
#' }
#' @references Sperling, E. A., et al. 2015 The Ecological Physiology of Earth's Second Oxygen Revolution.
#' @import dplyr
#' @importFrom rlang .data
#' @export
cGENIE.bw.redox <- function(experiment,
                            anox.thresh = 0,
                            subox.thresh = 4.8e-6,
                            lower.lims = TRUE) {
  
  # extract 2D bottom-water O2 field
  bw.O2 <- cGENIE.data(experiment = experiment,
                       var = "ocn_ben_O2",
                       dims = 2)
  
  # classify each cell
  bw.redox <- bw.O2 %>%
    mutate(redox.class = case_when(
      .data$var <= anox.thresh  ~ "anoxic",
      .data$var <= subox.thresh ~ "suboxic",
      .data$var >  subox.thresh ~ "oxic"
    )) %>%
    mutate(redox.class = factor(.data$redox.class,
                                levels = c("anoxic", "suboxic", "oxic")))
  
  # fraction of ocean cells in each class
  bw.redox.sum <- bw.redox %>%
    filter(!is.na(.data$redox.class)) %>%         # drop land / NA cells
    count(.data$redox.class, .drop = FALSE) %>%   # .drop = FALSE keeps classes with zero cells
    mutate(f.sink = .data$n / sum(.data$n))
  
  # extract individual outputs
  f.anox  <- bw.redox.sum$f.sink[bw.redox.sum$redox.class == "anoxic"]
  f.subox <- bw.redox.sum$f.sink[bw.redox.sum$redox.class == "suboxic"]
  f.ox    <- bw.redox.sum$f.sink[bw.redox.sum$redox.class == "oxic"]
  
  # Apply a lower limit to all seafloor fractions (0.01%) if `lower.lims` is TRUE,
  # then take the excess off the largest sink so the fractions still sum to 1
  if (lower.lims == TRUE) {
    f.min <- 0.0001
    f <- pmax(c(anox = f.anox, subox = f.subox, ox = f.ox), f.min)
    
    largest <- which.max(f)
    f[largest] <- f[largest] - (sum(f) - 1)
    
    f.anox  <- f[["anox"]]
    f.subox <- f[["subox"]]
    f.ox    <- f[["ox"]]
    
    stopifnot(isTRUE(all.equal(f.anox + f.subox + f.ox, 1)))
  }
  
  # Create a list to store and return the redox summary
  bw.redox.sum <- list(f.anox = f.anox, f.subox = f.subox, f.ox = f.ox)
  
  return(bw.redox.sum)
}
