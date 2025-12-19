#' Convert NetCDF Grid Cell Data Frame to sf Polygons
#'
#' Converts a data frame describing gridded NetCDF model output
#' (with longitude and latitude bounds) into an `sf` polygon object
#' suitable for spatial plotting.
#'
#' This function is designed to work with the output of
#' ' \code{\link{cGENIE.data}} after cleaning with \code{\link{clean.nc.df}}, but is
#' model-agnostic and can be used with any model that provides
#' rectangular grid-cell bounds.
#'
#' @param df A data frame containing grid-cell bounds. Must include
#'   `lon.min`, `lon.max`, `lat.min`, `lat.max`, and at least one data
#'   column (e.g. `var`).
#' @param var.col Character. Name of the column to attach as polygon
#'   attributes (default `"var"`).
#' @param crs Integer or character. Coordinate reference system for
#'   the output sf object (default EPSG:4326).
#'
#' @return An `sf` object with one polygon per grid cell and attributes
#'   inherited from the input data frame.
#'
#' @details
#' Each row of `df` is converted into a rectangular polygon defined by
#' its longitude and latitude bounds. Attribute data are preserved and
#' attached to the resulting spatial object.
#'
#' This function intentionally uses `sp` internally for compatibility
#' with existing cGENIE workflows, but returns a modern `sf` object.
#'
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom sf st_as_sf st_crs
#'
#' @export
nc.df.to.sf <- function(df,
                        var.col = "var",
                        crs = 4326) {

  required.cols <- c("lon.min", "lon.max", "lat.min", "lat.max", var.col)
  missing.cols <- setdiff(required.cols, names(df))
  if (length(missing.cols) > 0) {
    stop(
      "Input data frame is missing required columns: ",
      paste(missing.cols, collapse = ", ")
    )
  }

  poly.list <- vector("list", nrow(df))
  poly.ids  <- paste0("p", seq_len(nrow(df)))

  for (i in seq_len(nrow(df))) {
    coords <- cbind(
      c(df$lon.min[i], df$lon.max[i], df$lon.max[i], df$lon.min[i]),
      c(df$lat.min[i], df$lat.min[i], df$lat.max[i], df$lat.max[i])
    )

    poly.list[[i]] <- sp::Polygons(
      list(sp::Polygon(coords)),
      ID = poly.ids[i]
    )
  }

  SpP <- sp::SpatialPolygons(poly.list)

  attr.df <- data.frame(
    var = as.numeric(df[[var.col]]),  # force numeric
    row.names = poly.ids
  )

  SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr.df)

  sf.obj <- sf::st_as_sf(SpDf)
  sf::st_crs(sf.obj) <- crs

  sf.obj
}
