#' cGENIE Point Matching Benthic
#'
#' This function matches palaeocoordinates to cGENIE model data.
#'
#' @param var Character. The variable to extract from the cGENIE model. Not used if format is "array".
#' @param experiment Character. The experiment identifier for the cGENIE model. Not used if format is "array".
#' @param dims Numeric. The number of dimensions in the cGENIE model. Default is 3.
#' @param time.present Logical. Whether to use the present time for the cGENIE model. Default is FALSE.
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#' @param format Character. The format of the input data. Can be "nc" for netCDF or "array" for array format. Default is "nc".
#' @param input Character. The input data file name if the format is "array". Default is NULL. Not used if format is "nc".
#' @return A data frame with the original coordinates and matched climate data from the cGENIE model.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#' @export
cGENIE.point.matching.benthic <- function(var = NULL,
                                  experiment = NULL,
                                  format = "nc", # can be nc or array
                                  input = NULL,
                                  dims = 3,
                                  time.present = FALSE,
                                  coord.dat, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                                  lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                                  lng.name = "p_lng") # name IF generated from rotated paleoverse coordinates...
{
  # Load necessary libraries
  if(format == "nc"){
    # Extract grid data from cGENIE netCDF file
    grid.dat <- cGENIE.grid(experiment = experiment, dims = dims)

    # Extract climate data from cGENIE netCDF file
    clim.dat <- cGENIE.benthic.data(var = var, experiment = experiment, dims = dims, year = "default")
  } else if(format == "array"){
    # if using an array – use default cGENIE dims
    grid.dat <- list(
      lat = c(-76.463797, -66.443536, -59.441568, -53.663942, -48.590378, -43.982963,
              -39.709017, -35.685335, -31.855431, -28.178643, -24.624318, -21.168449,
              -17.791591, -14.477512, -11.212271,  -7.983556,  -4.780192,  -1.591754,
              1.591754,   4.780192,   7.983556,  11.212271,  14.477512,  17.791591,
              21.168449,  24.624318,  28.178643,  31.855431,  35.685335,  39.709017,
              43.982963,  48.590378,  53.663942,  59.441568,  66.443536,  76.463797),
      lat.edges = c(
        -90.000000, -70.811864, -62.733956, -56.442690, -51.057559, -46.238257,
        -41.810315, -37.669887, -33.748989, -30.000000, -26.387800, -22.885380,
        -19.471221, -16.127620, -12.839588,  -9.594068,  -6.379370,  -3.184739,
        0.000000,   3.184739,   6.379370,   9.594068,  12.839588,  16.127620,
        19.471221,  22.885380,  26.387800,  30.000000,  33.748989,  37.669887,
        41.810315,  46.238257,  51.057559,  56.442690,  62.733956,  70.811864,
        90.000000),
      lon = c(-175, -165, -155, -145, -135, -125, -115, -105,  -95,  -85,  -75,  -65,  -55,  -45,  -35,
              -25,  -15,   -5,    5,   15,   25,   35,   45,   55,   65,   75,   85,   95,  105,  115,
              125,  135,  145,  155,  165,  175),
      lon.edges = c(
        -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60, -50, -40,
        -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110,
        120, 130, 140, 150, 160, 170, 180),
      depth = c(  40.42035,  127.55154,  228.77023,  346.35409,  482.94908,  641.62894,
                  825.96438, 1040.10344, 1288.86481, 1577.84627, 1913.55066, 2303.53222,
                  2756.56654, 3282.84810, 3894.21960, 4604.43852),
      depth.edges = c(   0.00000,   80.84071,  174.75186,  283.84670,  410.58014,  557.80403,
                         728.83129,  927.51048, 1158.31240, 1426.43070, 1737.89874, 2099.72539,
                         2520.05268, 3008.33908, 3575.57232, 4234.51663, 5000.00000)
    )

    var.arr <- input
    lat <- grid.dat$lat
    lat.edges <- grid.dat$lat.edges
    lon <- grid.dat$lon
    lon.edges <- grid.dat$lon.edges

    if(mean(dplyr::between(lon, -180, 180)) < 1){
      lon.edges[lon.edges <= -180] <- lon.edges[lon.edges <= -180] + 360
      lon[lon <= -180] <- lon[lon <= -180] + 360
    }

    lon.length <- length(lon)
    lat.length <- length(lat)

    bw.cells <- array(dim = c(lon.length, lat.length))
    bw.array <- array(dim = c(lon.length, lat.length))

    depth_counts <- apply(var.arr, c(1, 2), function(x) sum(!is.na(x)))
    depth_levels <- 1:16

    for(lon.step in 1:lon.length){
      for(lat.step in 1:lat.length){
        if(depth_counts[lon.step, lat.step] == 0) {
          bw.cells[lon.step, lat.step] <- 0
          bw.array[lon.step, lat.step] <- NA
        } else {
          bw.cells[lon.step, lat.step] <- which.min(abs(depth_counts[lon.step, lat.step] - depth_levels)) - 1
          bw.array[lon.step, lat.step] <- var.arr[lon.step, lat.step, which.min(abs(depth_counts[lon.step, lat.step] - depth_levels)) - 1]
        }
      }
    }

    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      as.data.frame(reshape2::melt(bw.array))$value))

    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")

    df <- df %>%
      dplyr::filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    df$lon.range <- abs(df$lon.min - df$lon.max)
    df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
    df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

    clim.dat <- df
  }

  clim.dat <- dplyr::filter(clim.dat, !is.na(var))
  coord.dat <- dplyr::filter(coord.dat, !is.na(!!rlang::sym(lng.name)) & !is.na(!!rlang::sym(lat.name)))

  coord.dat$matched_climate <- NA

  for(row in 1:nrow(coord.dat)){
    coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat[[lat.name]][row] - grid.dat$lat))]

    lat.mid.opts <- clim.dat %>%
      dplyr::filter(lat.mid == coord.dat$lat.bin.mid[row])

    if(nrow(lat.mid.opts) > 0 & min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid)) < 10){
      coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid))]
      coord.dat$matched_climate[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row]]
    } else {
      coord.dat$lon.bin.mid[row] <- NA
      coord.dat$matched_climate[row] <- NA
    }
  }

  coord.dat <- dplyr::filter(coord.dat, is.na(matched_climate) == FALSE)

  return(coord.dat)

}
