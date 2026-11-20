#' Flag cGENIE grid cells that are adjacent to land
#'
#' For every cell in a cGENIE lon/lat cell data frame, finds the neighbouring
#' cell to the east, south, west and north and records whether each one is
#' land (i.e. its value is \code{NA}). An ocean cell is flagged as adjacent to
#' land if at least one of its four neighbours is land.
#'
#' @details
#' Neighbours are located from integer grid indices derived from the unique
#' \code{lon.mid} and \code{lat.mid} values, so the (unequal) latitude band
#' widths of the cGENIE grid do not matter. Longitude is treated as cyclic by
#' default, so the cell east of the easternmost column is the westernmost
#' column. Latitude is never wrapped: the southernmost row has no southern
#' neighbour and the northernmost row has no northern neighbour (reported as
#' \code{NA} and ignored when deciding whether a cell is adjacent to land).
#'
#' Any \code{NA} in \code{var} is treated as land. If \code{df} holds a single
#' depth level, \code{NA} also covers cells where the seafloor is shallower
#' than that level, so "adjacent to land" then means "adjacent to cells with
#' no ocean at this level". Use the surface layer (or a land mask) for true
#' coastlines. Rows missing from \code{df} are also treated as land (with a
#' warning).
#'
#' @param df Data frame with one row per grid cell and columns \code{lon.mid},
#'   \code{lat.mid} and the variable named by \code{var}. Must contain a single
#'   level / time slice only (one row per cell).
#' @param var Name of the column whose \code{NA}s define land. Default
#'   \code{"var"}.
#' @param wrap.lon Logical; treat longitude as cyclic? Default \code{TRUE}.
#' @param digits Number of decimal places used when matching cell midpoints,
#'   to guard against floating point differences. Default 6.
#'
#' @return \code{df} with five added columns: \code{land.E}, \code{land.S},
#'   \code{land.W}, \code{land.N} (\code{TRUE} if that neighbour is land,
#'   \code{FALSE} if ocean, \code{NA} if there is no neighbour) and
#'   \code{adj.land} (\code{TRUE} for ocean cells with at least one land
#'   neighbour, otherwise \code{FALSE}). Existing columns with these names are
#'   overwritten.
#'
#' @examples
#' # toy 36 x 36 grid with a block of land
#' grid <- expand.grid(lon.mid = seq(-175, 175, by = 10),
#'                     lat.mid = seq(-87.5, 87.5, by = 5))
#' grid$var <- 1
#' grid$var[grid$lon.mid > 0 & grid$lon.mid < 50 &
#'            grid$lat.mid > 0 & grid$lat.mid < 30] <- NA
#' out <- cGENIE.adjacent.land(grid)
#' table(out$adj.land)
#'
#' @export
cGENIE.adjacent.land <- function(df, var = "var", wrap.lon = TRUE, digits = 6) {

  # ---- checks ---------------------------------------------------------------
  if (!is.data.frame(df)) stop("`df` must be a data frame.")
  need <- c("lon.mid", "lat.mid", var)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop("`df` is missing column(s): ", paste(miss, collapse = ", "))
  }
  if (anyNA(df$lon.mid) || anyNA(df$lat.mid)) {
    stop("`lon.mid` and `lat.mid` must not contain NA.")
  }

  # ---- grid indices ---------------------------------------------------------
  lon  <- round(df$lon.mid, digits)
  lat  <- round(df$lat.mid, digits)
  lons <- sort(unique(lon))
  lats <- sort(unique(lat))
  nx   <- length(lons)
  ny   <- length(lats)
  ix   <- match(lon, lons)   # 1 = westernmost column
  iy   <- match(lat, lats)   # 1 = southernmost row

  if (anyDuplicated(cbind(iy, ix)) > 0) {
    stop("More than one row per grid cell: `df` must contain a single ",
         "level / time slice. Split it first (e.g. with split()) and apply ",
         "the function to each piece.")
  }
  if (nrow(df) < nx * ny) {
    warning("`df` does not cover the full lon/lat grid; missing cells are ",
            "treated as land.")
  }

  # ---- land matrix (rows = lat S->N, cols = lon W->E) -----------------------
  land <- matrix(TRUE, nrow = ny, ncol = nx)
  land[cbind(iy, ix)] <- is.na(df[[var]])

  # neighbour lookup: TRUE/FALSE if the neighbour exists, NA if off the grid
  look <- function(jy, jx) {
    ok  <- !is.na(jy) & !is.na(jx) & jy >= 1L & jy <= ny & jx >= 1L & jx <= nx
    out <- rep(NA, length(jy))
    out[ok] <- land[cbind(jy[ok], jx[ok])]
    out
  }

  # ---- neighbour indices ----------------------------------------------------
  ix.e <- ix + 1L
  ix.w <- ix - 1L
  if (wrap.lon) {
    ix.e[ix.e > nx] <- 1L
    ix.w[ix.w < 1L] <- nx
  }

  nb <- cbind(
    land.E = look(iy,      ix.e),
    land.S = look(iy - 1L, ix),
    land.W = look(iy,      ix.w),
    land.N = look(iy + 1L, ix)
  )

  # ---- output ---------------------------------------------------------------
  df$land.E <- nb[, "land.E"]
  df$land.S <- nb[, "land.S"]
  df$land.W <- nb[, "land.W"]
  df$land.N <- nb[, "land.N"]
  df$adj.land <- !is.na(df[[var]]) & rowSums(nb, na.rm = TRUE) > 0

  df
}
