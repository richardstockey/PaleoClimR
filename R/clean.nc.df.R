#' Clean gridded NetCDF data frames for spatial polygon plotting
#'
#' This function prepares gridded model output extracted from NetCDF files
#' for safe polygon-based spatial plotting. It normalises longitude
#' conventions, repairs or splits grid cells that cross the dateline,
#' and removes invalid geographic ranges.
#'
#' The function is model-agnostic and is designed to work with output from
#' \code{cGENIE.data()}, \code{nc.list.to.df()}, or similar NetCDF extraction
#' workflows. It handles common longitude conventions including
#' \code{-180–180}, \code{0–360}, and mis-specified but topologically correct
#' grids (e.g. \code{-90–270}, \code{-260–90}).
#'
#' @param df A data frame containing grid-cell bounds with columns
#'   \code{lon.min}, \code{lon.max}, \code{lat.min}, \code{lat.max}.
#'   Optional columns (e.g. \code{lon.mid}, \code{lat.mid}, data variables)
#'   are preserved.
#' @param lon.range Numeric length-2 vector giving the target longitude range
#'   for plotting (default \code{c(-180, 180)}).
#' @param lat.range Numeric length-2 vector giving the valid latitude range
#'   (default \code{c(-90, 90)}).
#' @param split.dateline Logical; if TRUE (default), grid cells that cross
#'   the dateline are split into two valid polygons. If FALSE, such cells
#'   are dropped.
#' @param verbose Logical; if TRUE, print messages describing corrections.
#'
#' @return A cleaned data frame suitable for polygon-based spatial plotting.
#'   Dateline-crossing grid cells may be duplicated and split into two rows.
#'
#' @details
#' Longitude normalisation is performed using modular arithmetic to preserve
#' grid topology. Grid cells whose longitudinal extent exceeds half the
#' plotting range are interpreted as crossing the dateline. When
#' \code{split.dateline = TRUE}, these cells are split into two valid
#' longitude intervals on either side of the dateline.
#'
#' @export
clean.nc.df <- function(df,
                        lon.range = c(-180, 180),
                        lat.range = c(-90, 90),
                        split.dateline = TRUE,
                        verbose = FALSE) {

  required <- c("lon.min", "lon.max", "lat.min", "lat.max")
  if (!all(required %in% names(df))) {
    stop("df must contain columns: ",
         paste(required, collapse = ", "))
  }

  # ---- Helper: wrap longitudes into target range ----
  wrap_lon <- function(x) {
    ((x - lon.range[1]) %% diff(lon.range)) + lon.range[1]
  }

  # ---- Step 1: Normalise longitude convention ----
  lon_vals <- c(df$lon.min, df$lon.max)

  if (min(lon_vals, na.rm = TRUE) < lon.range[1] ||
      max(lon_vals, na.rm = TRUE) > lon.range[2]) {

    if (verbose) {
      message("Normalising longitude range to ",
              paste(lon.range, collapse = " to "))
    }

    df$lon.min <- wrap_lon(df$lon.min)
    df$lon.max <- wrap_lon(df$lon.max)

    if ("lon.mid" %in% names(df)) {
      df$lon.mid <- wrap_lon(df$lon.mid)
    }
  }

  # ---- Step 2: Identify dateline-crossing cells ----
  lon.width <- abs(df$lon.max - df$lon.min)
  crosses.dateline <- lon.width > diff(lon.range) / 2

  if (any(crosses.dateline, na.rm = TRUE)) {

    if (!split.dateline) {
      if (verbose) {
        message("Dropping ", sum(crosses.dateline),
                " dateline-crossing grid cells")
      }
      df <- df[!crosses.dateline, , drop = FALSE]

    } else {

      if (verbose) {
        message("Splitting ", sum(crosses.dateline),
                " dateline-crossing grid cells")
      }

      df_keep <- df[!crosses.dateline, , drop = FALSE]
      df_split <- df[crosses.dateline, , drop = FALSE]

      # Left-hand polygon (lon.min -> +180)
      left <- df_split
      left$lon.max <- lon.range[2]

      # Right-hand polygon (-180 -> lon.max)
      right <- df_split
      right$lon.min <- lon.range[1]

      # Update midpoints if present
      if ("lon.mid" %in% names(df)) {
        left$lon.mid  <- (left$lon.min + left$lon.max) / 2
        right$lon.mid <- (right$lon.min + right$lon.max) / 2
      }

      df <- rbind(df_keep, left, right)
    }
  }

  # ---- Step 3: Remove invalid geographic ranges ----
  valid <- df$lon.min >= lon.range[1] &
    df$lon.max <= lon.range[2] &
    df$lat.min >= lat.range[1] &
    df$lat.max <= lat.range[2]

  n_bad <- sum(!valid, na.rm = TRUE)
  if (n_bad > 0 && verbose) {
    message("Dropping ", n_bad, " grid cells outside valid geographic bounds")
  }

  df <- df[valid, , drop = FALSE]

  rownames(df) <- NULL
  df
}
