#' Extract a Latitude or Longitude Transect Slice
#'
#' Utility function to extract the nearest latitude or longitude slice
#' from gridded data and define plotting bounds for transect plots.
#'
#' @param df Dataframe containing gridded model output with lat/lon columns.
#' @param degrees Numeric value specifying the target latitude or longitude.
#' @param orientation Character string: "lat" or "lon".
#' @param upper.bound Numeric upper y-axis bound (typically shallow depth).
#' @param lower.bound Numeric lower y-axis bound (typically deep ocean).
#'
#' @return A list containing:
#' \itemize{
#'   \item df.slice: Filtered dataframe for the transect
#'   \item band.value: Numeric value of the selected lat/lon
#'   \item band.label: Rounded value for plotting labels
#'   \item box: Dataframe defining plot bounding box
#'   \item xlims: Numeric vector of x-axis limits
#' }
#' @export
gen.transect.slice <- function(df,
                               degrees = 0,
                               orientation = "lat",
                               upper.bound,
                               lower.bound) {

  if (!orientation %in% c("lat", "lon")) {
    stop("orientation must be 'lat' or 'lon'")
  }

  if (orientation == "lon") {
    # Select latitude slice
    lat.bands <- sort(unique(df[["lat.mid"]]))
    band <- lat.bands[which.min(abs(lat.bands - degrees))]
    band.label <- round(band, 1)

    df.slice <- dplyr::filter(df, .data[["lat.mid"]] == band)
    xlims <- c(-180, 180)

  } else {
    # Select longitude slice
    lon.bands <- sort(unique(df[["lon.mid"]]))
    band <- lon.bands[which.min(abs(lon.bands - degrees))]
    band.label <- round(band, 1)

    df.slice <- dplyr::filter(df, .data[["lon.mid"]] == band)
    xlims <- c(-90, 90)
  }

  # Define bounding box
  box <- data.frame(
    xmin = xlims[1],
    xmax = xlims[2],
    ymin = upper.bound,
    ymax = lower.bound
  )

  return(list(
    df.slice = df.slice,
    band.value = band,
    band.label = band.label,
    box = box,
    xlims = xlims
  ))
}
