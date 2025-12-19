#' Extract Grid Data from cGENIE NetCDF Files
#'
#' This function extracts grid-related data (latitude, longitude, and depth)
#' from a specified cGENIE NetCDF file. It handles both 2D and 3D grid data
#' based on the specified dimensions. If `experiment` or `dims` is `NULL`,
#' it returns the default 36x36x16 cGENIE grid dimensions.
#'
#' @param experiment Character string indicating the path to the cGENIE experiment directory.
#' @param dims Integer specifying the dimensions of the NetCDF file to read. Default is NULL (returns default grid).
#' @param model Character string indicating the model type (default "biogem"). Currently only "biogem" is supported.
#' @return A list containing latitude, longitude, depth (if dims=3), and their respective edges.
#' @details The function adjusts longitude values to be within -180 to 180 and reports if any values were outside this range.
#'
#' @importFrom RNetCDF open.nc var.get.nc
#' @export
cGENIE.grid <- function(experiment = NULL,
                        dims = NULL,
                        model = "biogem") {

  # Check model
  if (model != "biogem") {
    stop("Currently only 'biogem' model is supported.")
  }
  prefix <- "/biogem/fields_biogem_"

  # Default grid if experiment or dims is NULL
  if (is.null(experiment) || is.null(dims)) {
    message("experiment or dims is NULL, returning default 36x36x16 cGENIE grid dimensions")

    lat <- c(-76.463797, -66.443536, -59.441568, -53.663942, -48.590378, -43.982963,
             -39.709017, -35.685335, -31.855431, -28.178643, -24.624318, -21.168449,
             -17.791591, -14.477512, -11.212271,  -7.983556,  -4.780192,  -1.591754,
             1.591754,   4.780192,   7.983556,  11.212271,  14.477512,  17.791591,
             21.168449,  24.624318,  28.178643,  31.855431,  35.685335,  39.709017,
             43.982963,  48.590378,  53.663942,  59.441568,  66.443536,  76.463797)

    lat.edges <- c(-90.000000, -70.811864, -62.733956, -56.442690, -51.057559, -46.238257,
                   -41.810315, -37.669887, -33.748989, -30.000000, -26.387800, -22.885380,
                   -19.471221, -16.127620, -12.839588,  -9.594068,  -6.379370,  -3.184739,
                   0.000000,   3.184739,   6.379370,   9.594068,  12.839588,  16.127620,
                   19.471221,  22.885380,  26.387800,  30.000000,  33.748989,  37.669887,
                   41.810315,  46.238257,  51.057559,  56.442690,  62.733956,  70.811864,
                   90.000000)

    lon <- seq(-175, 175, by = 10)
    lon.edges <- seq(-180, 180, by = 10)

    depth <- c(  40.42035,  127.55154,  228.77023,  346.35409,  482.94908,  641.62894,
                 825.96438, 1040.10344, 1288.86481, 1577.84627, 1913.55066, 2303.53222,
                 2756.56654, 3282.84810, 3894.21960, 4604.43852)
    depth.edges <- c(0.00000, 80.84071, 174.75186, 283.84670, 410.58014, 557.80403,
                     728.83129, 927.51048, 1158.31240, 1426.43070, 1737.89874, 2099.72539,
                     2520.05268, 3008.33908, 3575.57232, 4234.51663, 5000.00000)

    if (is.null(dims)) dims <- 3  # assume 3D default

    grid <- if (dims == 3) {
      list(lat = lat, lat.edges = lat.edges,
           lon = lon, lon.edges = lon.edges,
           depth = depth, depth.edges = depth.edges)
    } else {
      list(lat = lat, lat.edges = lat.edges,
           lon = lon, lon.edges = lon.edges)
    }

    return(grid)
  }

  # Otherwise, read from NetCDF file
  nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

  lat <- RNetCDF::var.get.nc(nc, "lat")
  lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")
  lon <- RNetCDF::var.get.nc(nc, "lon")
  lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")
  depth <- if (dims == 3) RNetCDF::var.get.nc(nc, "zt") else NULL
  depth.edges <- if (dims == 3) RNetCDF::var.get.nc(nc, "zt_edges") else NULL

  # ---- wrap longitudes to [-180, 180] and report if out-of-range ----
  wrap_lon <- function(x) ((x + 180) %% 360) - 180
  if (any(lon < -180 | lon > 180)) {
    message("Longitude values outside [-180, 180] detected. Wrapping them.")
    lon <- wrap_lon(lon)
  }
  if (any(lon.edges < -180 | lon.edges > 180)) {
    message("Longitude edges outside [-180, 180] detected. Wrapping them.")
    lon.edges <- wrap_lon(lon.edges)
  }

  # Return grid list
  grid <- if (dims == 3) {
    list(lat = lat, lat.edges = lat.edges,
         lon = lon, lon.edges = lon.edges,
         depth = depth, depth.edges = depth.edges)
  } else {
    list(lat = lat, lat.edges = lat.edges,
         lon = lon, lon.edges = lon.edges)
  }

  return(grid)
}
