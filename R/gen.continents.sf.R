#' Generate continental outlines from cleaned model output
#'
#' Converts grid cells with NA values (assumed land) into a single
#' `sf` object representing continental outlines.
#'
#' @param df Dataframe from `clean.nc.df()` containing at least columns:
#'           lon.min, lon.max, lat.min, lat.max, var
#' @return An `sf` object representing continental outlines
#' @export

gen.continents.sf <- function(df) {
  if (!is.data.frame(df)) stop("Input must be a dataframe from clean.nc.df()")
  if (!all(c("lon.min","lon.max","lat.min","lat.max","var") %in% names(df))) {
    stop("Dataframe must contain lon.min, lon.max, lat.min, lat.max, var columns")
  }

  # Select land cells (NA values)
  continent_polygons <- df[is.na(df$var), , drop = FALSE]

  if(nrow(continent_polygons) == 0) {
    warning("No NA cells found; returning empty sf object")
    return(sf::st_sf())
  }

  # Create list of sp::Polygons
  poly.list <- lapply(1:nrow(continent_polygons), function(i) {
    coords <- cbind(
      c(continent_polygons$lon.min[i], continent_polygons$lon.max[i],
        continent_polygons$lon.max[i], continent_polygons$lon.min[i]),
      c(continent_polygons$lat.min[i], continent_polygons$lat.min[i],
        continent_polygons$lat.max[i], continent_polygons$lat.max[i])
    )
    sp::Polygons(list(sp::Polygon(coords)), ID = paste0("p", i))
  })

  # Convert to SpatialPolygonsDataFrame
  SpP <- sp::SpatialPolygons(poly.list)
  attr <- data.frame(row.names = sapply(poly.list, function(x) x@ID))
  SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)

  # Convert to sf and union into single polygon
  SpDfSf <- sf::st_as_sf(SpDf)
  sf::st_crs(SpDfSf) <- 4326
  continents_union <- sf::st_union(SpDfSf)

  return(continents_union)
}
