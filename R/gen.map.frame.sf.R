#' Generate a frame polygon for a map
#'
#' Creates an `sf` polygon representing the outline of the map.
#' Useful for adding a border/frame around global map plots.
#'
#' @param lon.range Numeric vector of length 2. Longitude bounds (default c(-180, 180)).
#' @param lat.range Numeric vector of length 2. Latitude bounds (default c(-90, 90)).
#' @param n.steps Numeric. Number of steps along the edges for smooth frame (default 1800).
#' @return An `sf` object representing the frame polygon.
#' @export
gen.map.frame.sf <- function(lon.range = c(-180, 180), lat.range = c(-90, 90), n.steps = 1800) {

  # Create coordinates tracing the outer rectangle
  lons <- c(
    lon.range[1],
    lon.range[2],
    rep(lon.range[2], n.steps),
    lon.range[2],
    lon.range[1],
    rep(lon.range[1], n.steps),
    lon.range[1]
  )

  lats <- c(
    lat.range[1],
    lat.range[1],
    seq(lat.range[1], lat.range[2], length.out = n.steps),
    lat.range[2],
    lat.range[2],
    seq(lat.range[2], lat.range[1], length.out = n.steps),
    lat.range[1]
  )

  # Create sp polygon and convert to sf
  frame_poly <- sp::Polygon(cbind(lons, lats))
  frame_sp   <- sp::SpatialPolygons(list(sp::Polygons(list(frame_poly), ID = "frame")))
  frame_spdf <- sp::SpatialPolygonsDataFrame(frame_sp, data = data.frame(var = 1, row.names = "frame"))
  frame_sf   <- sf::st_as_sf(frame_spdf)
  sf::st_crs(frame_sf) <- 4326

  return(frame_sf)
}
