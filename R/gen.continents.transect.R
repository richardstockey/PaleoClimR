
#' Generate continental outline segments for transect plots
#'
#' Identifies the outer edges of contiguous "land" cells (defined as `NA`
#' values in a specified variable) within a gridded transect dataset, and
#' returns line segments suitable for plotting with `geom_segment()`.
#'
#' This function operates directly on a rectangular grid (e.g. output from
#' `clean.nc.df()`) and constructs the outline by checking for neighbouring
#' cells in the horizontal and vertical directions. Edges are only drawn
#' where a land cell borders a non-land cell (or the edge of the grid).
#'
#' Designed for use in transect-style plots (e.g. latitude–depth or
#' longitude–depth sections) using a Cartesian coordinate system.
#'
#' @param df A dataframe containing gridded model output. Must include:
#' \describe{
#'   \item{lat.min, lat.max, lat.mid}{Latitude bounds and midpoint}
#'   \item{lon.min, lon.max, lon.mid}{Longitude bounds and midpoint}
#'   \item{depth.min, depth.max, depth}{Depth bounds and midpoint}
#'   \item{var}{Variable used to identify land (via `NA` values)}
#' }
#' @param var Character string giving the column name used to identify land
#' cells (`NA` values are treated as land).
#' @param orientation Character string specifying transect orientation:
#' \describe{
#'   \item{"lat"}{Latitude–depth transect (default)}
#'   \item{"lon"}{Longitude–depth transect}
#' }
#'
#' @return A dataframe of line segments with columns:
#' \describe{
#'   \item{x, y}{Start coordinates of segment}
#'   \item{xend, yend}{End coordinates of segment}
#' }
#' suitable for plotting with `ggplot2::geom_segment()`.
#'
#' @details
#' The function assumes a regular grid and detects neighbouring cells based
#' on matching midpoints in the horizontal direction and adjacent depth levels.
#' Each land cell is evaluated independently, and edges are added only where
#' no adjacent land cell exists in that direction.
#'
#' The resulting segments may include duplicates along shared edges; these
#' typically do not affect plotting but could be removed with post-processing
#' if needed.
#' @export
gen.continents.transect <- function(df, var, orientation = "lat") {

  if (!is.data.frame(df)) {
    stop("df must be a dataframe")
  }

  if (!(var %in% names(df))) {
    stop("var must be a column in df")
  }

  # Identify land
  df$land <- is.na(df[[var]])

  # Sort for consistent neighbour detection
  if (orientation == "lat") {
    df <- df[order(df$lat.mid, df$depth), ]
  } else {
    df <- df[order(df$lon.mid, df$depth), ]
  }

  segments <- list()
  seg_id <- 1

  for (i in seq_len(nrow(df))) {

    if (!df$land[i]) next  # only care about land cells

    cell <- df[i, ]

    # Define neighbours (same lat/lon band, adjacent depth or horizontal)
    neighbours <- df[
      (abs(df$depth - cell$depth) <= diff(range(df$depth))/length(unique(df$depth))) &
        (
          abs(df$lat.mid - cell$lat.mid) <= diff(range(df$lat.mid))/length(unique(df$lat.mid)) |
            abs(df$lon.mid - cell$lon.mid) <= diff(range(df$lon.mid))/length(unique(df$lon.mid))
        ),
    ]

    # --- TOP edge ---
    above <- df[df$depth < cell$depth &
                  ((orientation == "lat" & df$lat.mid == cell$lat.mid) |
                     (orientation == "lon" & df$lon.mid == cell$lon.mid)), ]

    if (nrow(above) == 0 || !above$land[which.max(above$depth)]) {
      segments[[seg_id]] <- data.frame(
        x = if (orientation == "lat") c(cell$lat.min, cell$lat.max) else c(cell$lon.min, cell$lon.max),
        y = c(cell$depth.min, cell$depth.min)
      )
      seg_id <- seg_id + 1
    }

    # --- BOTTOM edge ---
    below <- df[df$depth > cell$depth &
                  ((orientation == "lat" & df$lat.mid == cell$lat.mid) |
                     (orientation == "lon" & df$lon.mid == cell$lon.mid)), ]

    if (nrow(below) == 0 || !below$land[which.min(below$depth)]) {
      segments[[seg_id]] <- data.frame(
        x = if (orientation == "lat") c(cell$lat.min, cell$lat.max) else c(cell$lon.min, cell$lon.max),
        y = c(cell$depth.max, cell$depth.max)
      )
      seg_id <- seg_id + 1
    }

    # --- LEFT edge ---
    if (orientation == "lat") {
      left <- df[df$lat.mid < cell$lat.mid & df$depth == cell$depth, ]
    } else {
      left <- df[df$lon.mid < cell$lon.mid & df$depth == cell$depth, ]
    }

    if (nrow(left) == 0 || !left$land[which.max(if (orientation == "lat") left$lat.mid else left$lon.mid)]) {
      segments[[seg_id]] <- data.frame(
        x = if (orientation == "lat") rep(cell$lat.min, 2) else rep(cell$lon.min, 2),
        y = c(cell$depth.min, cell$depth.max)
      )
      seg_id <- seg_id + 1
    }

    # --- RIGHT edge ---
    if (orientation == "lat") {
      right <- df[df$lat.mid > cell$lat.mid & df$depth == cell$depth, ]
    } else {
      right <- df[df$lon.mid > cell$lon.mid & df$depth == cell$depth, ]
    }

    if (nrow(right) == 0 || !right$land[which.min(if (orientation == "lat") right$lat.mid else right$lon.mid)]) {
      segments[[seg_id]] <- data.frame(
        x = if (orientation == "lat") rep(cell$lat.max, 2) else rep(cell$lon.max, 2),
        y = c(cell$depth.min, cell$depth.max)
      )
      seg_id <- seg_id + 1
    }
  }

  if (length(segments) == 0) {
    return(data.frame())
  }

  seg_df <- do.call(rbind, lapply(segments, function(s) {
    data.frame(x = s$x[1], y = s$y[1], xend = s$x[2], yend = s$y[2])
  }))

  return(seg_df)
}
