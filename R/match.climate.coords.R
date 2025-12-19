#' Match Climate Data to Coordinates (Vectorized, cGENIE/HADCM3)
#'
#' Efficiently matches gridded climate data to a set of coordinates, handling NA cells.
#'
#' @param clim.dat Data frame with columns `lat.mid`, `lon.mid`, `var`.
#' @param grid.dat List from `cGENIE.grid()` or equivalent HADCM3 grid function.
#' @param coord.dat Data frame with coordinates to match climate data.
#' @param lat.name Name of latitude column in coord.dat (default "p_lat").
#' @param lng.name Name of longitude column in coord.dat (default "p_lng").
#' @param max_dist Maximum allowed distance for matching (km, default 1000 km).
#' @param na.method How to handle NA climate values: "nearest" (default), "same.lat", "keep".
#' @param verbose Logical; if TRUE, print summary statistics (default TRUE).
#' @return coord.dat with new columns: `matched_climate`, `lat.bin.mid`, `lon.bin.mid`.
#' @importFrom dplyr filter
#' @export
match.climate.coords <- function(clim.dat,
                                 grid.dat,
                                 coord.dat,
                                 lat.name = "p_lat",
                                 lng.name = "p_lng",
                                 max_dist = 1000,
                                 na.method = c("nearest", "same.lat", "keep"),
                                 verbose = TRUE) {

  na.method <- match.arg(na.method)

  # ---- Filter out coordinates with missing lat/lon ----
  coord.dat <- dplyr::filter(coord.dat,
                             !is.na(.data[[lat.name]]),
                             !is.na(.data[[lng.name]]))

  n_total <- nrow(coord.dat)

  # ---- Wrap longitudes into -180:180 ----
  wrap_lon <- function(x) ((x + 180) %% 360) - 180
  coord.dat[[lng.name]] <- wrap_lon(coord.dat[[lng.name]])
  clim.dat$lon.mid <- wrap_lon(clim.dat$lon.mid)

  # ---- Haversine distance function ----
  haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 6371  # Earth radius in km
    delta_lat <- (lat2 - lat1) * pi / 180
    delta_lon <- (lon2 - lon1) * pi / 180
    a <- sin(delta_lat/2)^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*sin(delta_lon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c
  }

  # ---- Initialize result columns ----
  coord.dat$lat.bin.mid <- NA
  coord.dat$lon.bin.mid <- NA
  coord.dat$matched_climate <- NA

  # ---- Assign initial nearest lat/lon bins ----
  lat_idx <- sapply(coord.dat[[lat.name]], function(x) which.min(abs(grid.dat$lat - x)))
  lon_idx <- sapply(coord.dat[[lng.name]], function(x) which.min(abs(grid.dat$lon - x)))
  coord.dat$lat.bin.mid <- grid.dat$lat[lat_idx]
  coord.dat$lon.bin.mid <- grid.dat$lon[lon_idx]

  # ---- Initial direct match ----
  clim_key <- paste(clim.dat$lat.mid, clim.dat$lon.mid)
  coord_key <- paste(coord.dat$lat.bin.mid, coord.dat$lon.bin.mid)
  coord.dat$matched_climate <- clim.dat$var[match(coord_key, clim_key)]

  # ---- Verbose land stats before NA handling ----
  na_rows <- which(is.na(coord.dat$matched_climate))
  n_land <- length(na_rows)

  if(verbose) {
    message(sprintf("Points on land / total: %d/%d (%.1f%%)", n_land, n_total, 100 * n_land / n_total))
  }

  # ---- Vectorized NA handling ----
  moved_distances_km <- numeric(n_land)

  if(length(na_rows) > 0 && na.method != "keep") {
    valid_idx <- which(!is.na(clim.dat$var))

    if(na.method == "nearest") {
      # Vectorized Haversine distances
      lat_mat <- outer(coord.dat[[lat.name]][na_rows], clim.dat$lat.mid[valid_idx], "-")
      lon_mat <- outer(coord.dat[[lng.name]][na_rows], clim.dat$lon.mid[valid_idx], "-")
      dist_mat <- haversine(
        matrix(rep(coord.dat[[lat.name]][na_rows], each = length(valid_idx)), nrow = length(na_rows)),
        matrix(rep(coord.dat[[lng.name]][na_rows], each = length(valid_idx)), nrow = length(na_rows)),
        matrix(rep(clim.dat$lat.mid[valid_idx], times = length(na_rows)), nrow = length(na_rows), byrow = TRUE),
        matrix(rep(clim.dat$lon.mid[valid_idx], times = length(na_rows)), nrow = length(na_rows), byrow = TRUE)
      )
      nearest_idx <- apply(dist_mat, 1, which.min)
      nearest_dist <- apply(dist_mat, 1, min)
      use_idx <- nearest_dist <= max_dist
      if(any(use_idx)) {
        coord.dat$matched_climate[na_rows[use_idx]] <- clim.dat$var[valid_idx[nearest_idx[use_idx]]]
        coord.dat$lat.bin.mid[na_rows[use_idx]] <- clim.dat$lat.mid[valid_idx[nearest_idx[use_idx]]]
        coord.dat$lon.bin.mid[na_rows[use_idx]] <- clim.dat$lon.mid[valid_idx[nearest_idx[use_idx]]]
        moved_distances_km[use_idx] <- nearest_dist[use_idx]
      }
    } else if(na.method == "same.lat") {
      for(i in seq_along(na_rows)) {
        idx <- na_rows[i]
        same_lat_idx <- which(clim.dat$lat.mid == coord.dat$lat.bin.mid[idx] & !is.na(clim.dat$var))
        if(length(same_lat_idx) > 0) {
          # Distance along longitude at given latitude
          dist_lon_km <- haversine(coord.dat[[lat.name]][idx],
                                   coord.dat[[lng.name]][idx],
                                   coord.dat[[lat.name]][idx],
                                   clim.dat$lon.mid[same_lat_idx])
          if(min(dist_lon_km) <= max_dist) {
            nearest <- same_lat_idx[which.min(dist_lon_km)]
            coord.dat$matched_climate[idx] <- clim.dat$var[nearest]
            coord.dat$lon.bin.mid[idx] <- clim.dat$lon.mid[nearest]
            moved_distances_km[i] <- min(dist_lon_km)
          }
        }
      }
    }
  }

  # ---- Verbose moved stats ----
  n_moved <- sum(moved_distances_km > 0)
  if(verbose) {
    if(n_moved > 0) {
      message(sprintf("Points moved / on land: %d/%d (%.1f%%)", n_moved, n_land, 100 * n_moved / n_land))
      message(sprintf("Distance moved (km): mean = %.1f, range = %.1f-%.1f",
                      mean(moved_distances_km[moved_distances_km > 0]),
                      min(moved_distances_km[moved_distances_km > 0]),
                      max(moved_distances_km[moved_distances_km > 0])))
    } else {
      message("No points were moved.")
    }
  }

  return(coord.dat)
}
