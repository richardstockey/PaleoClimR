#' cGENIE Point Matching
#'
#' This function matches palaeocoordinates to cGENIE model data.
#'
#' @param var Character. The variable to extract from the cGENIE model.
#' @param experiment Character. The experiment identifier for the cGENIE model.
#' @param depth.level Numeric. The depth level to extract from the cGENIE model. Default is 1.
#' @param dims Numeric. The number of dimensions in the cGENIE model. Default is 3.
#' @param time.present Logical. Whether to use the present time for the cGENIE model. Default is FALSE.
#' @param coord.dat Data frame. A data frame with latitude and longitude columns. cGENIE data will be added to this and returned.
#' @param lat.name Character. The name of the latitude column in `coord.dat`. Default is "p_lat".
#' @param lng.name Character. The name of the longitude column in `coord.dat`. Default is "p_lng".
#'
#' @return A data frame with the original coordinates and matched climate data from the cGENIE model.
#'
#' @import RNetCDF
#' @import dplyr
#' @import reshape2
#' @export

cGENIE.point.matching <- function(var = NULL,
                                  experiment = NULL,
                                  input = NULL,
                                  format = "nc",
                                  depth.level = 1,
                                  dims = 3,
                                  coord.dat = NULL, # is any data frame with the lat long column names assigned - cGENIE data will be added to this and returned
                                  lat.name = "p_lat", # name IF generated from rotated paleoverse coordinates...
                                  lng.name = "p_lng") # name IF generated from rotated paleoverse coordinates...
{

# Load necessary libraries
library(RNetCDF)  # For handling NetCDF files
library(dplyr)    # For data manipulation
library(reshape2) # For reshaping data

  if(format == "nc" | format == "array"){
if(format == "nc"){
# Extract grid data from cGENIE netCDF file
# This function call retrieves the grid data for the specified experiment and dimensions.
# The grid data contains information about the spatial layout of the cGENIE model.
grid.dat <- cGENIE.grid(experiment = experiment, dims = dims)

# Extract climate data from cGENIE netCDF file
# This function call retrieves the climate data for the specified variable, experiment, depth level, and dimensions.
# The 'year' parameter is set to "default" to use the default time slice.
clim.dat <- cGENIE.data(var = var, experiment = experiment, depth.level = depth.level, dims = dims, year = "default")
} else if(format == "array") {
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

    # then, use key elements of cGENIE.data to extract the same data format from the array
    var.arr <- input
    # Extract general variables
    lat <- grid.dat$lat            # Latitude (degrees north)
    lat.edges <- grid.dat$lat.edges # Latitude edges
    lon <- grid.dat$lon            # Longitude (degrees east)
    lon.edges <- grid.dat$lon.edges # Longitude edges

    df <- as.data.frame(cbind(
        rep(lon, times = length(lat), each = 1),
        rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
        rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
        rep(lat, times = 1, each = length(lon)),
        rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
        rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
        as.data.frame(melt(var.arr[,, depth.level]))$value
    ))

    names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")

    # Filter the data to ensure valid lat-lon ranges
    df <- df %>%
        filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    # Handle cells at the extremes (-180 and 180 longitude)
    df$lon.range <- abs(df$lon.min - df$lon.max)
    df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
    df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180]

    clim.dat <- df
}

# Omit NAs in the var value for climate data file
# This step filters out any rows in the climate data where the specified variable has NA values.
# This ensures that only valid data points are used i§n the matching process.
clim.dat <- filter(clim.dat, !is.na(var))
# Remove any NA paleocoordinates
# This step filters out any rows in the coordinate data where the latitude or longitude values are NA.
# This ensures that only valid coordinates are used in the matching process.
coord.dat <- filter(coord.dat, !is.na(!!sym(lng.name)) & !is.na(!!sym(lat.name)))

# Initialize a column for matched climate data
# This step adds a new column to the coordinate data frame to store the matched climate data.
# Initially, all values in this column are set to NA.
coord.dat$matched_climate <- NA

# Iterate over each row in the coordinate data frame
# This loop processes each coordinate point to find the matching climate data.
for(row in 1:nrow(coord.dat)){

    # Find the mid-point of the nearest latitudinal grid cell for each occurrence
    # This step identifies the closest latitude grid point in the cGENIE model to the current coordinate's latitude.
    coord.dat$lat.bin.mid[row] <- grid.dat$lat[which.min(abs(coord.dat[[lat.name]][row] - grid.dat$lat))]

    # Identify all the cells in the climate model that have the same latitude as the data point
    # This step filters the climate data to include only the rows where the latitude matches the closest latitude grid point.
    lat.mid.opts <- clim.dat %>%
        filter(lat.mid == coord.dat$lat.bin.mid[row])

    # Check if there are any matching latitude options and if the closest longitudinal bin is within 10 degrees
    # This condition ensures that there are valid latitude matches and that the closest longitude grid point is within a reasonable distance.
    if(nrow(lat.mid.opts) > 0 & min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid)) < 10){

        # Find the mid-point of the nearest longitudinal grid cell
        # This step identifies the closest longitude grid point in the cGENIE model to the current coordinate's longitude.
        coord.dat$lon.bin.mid[row] <- lat.mid.opts$lon.mid[which.min(abs(coord.dat[[lng.name]][row] - lat.mid.opts$lon.mid))]

        # Assign the matched climate data based on the assigned latitudinal and longitudinal bins
        # This step retrieves the climate data value for the closest latitude and longitude grid points and assigns it to the matched_climate column.
        coord.dat$matched_climate[row] <- clim.dat$var[clim.dat$lat.mid == coord.dat$lat.bin.mid[row] & clim.dat$lon.mid == coord.dat$lon.bin.mid[row]]

    } else {
        # If there are no valid latitude matches or the nearest longitude grid cell is more than 10 degrees away, assign NA
        # This step handles cases where the coordinate point is too far from any valid climate data points.
        coord.dat$lon.bin.mid[row] <- NA
        coord.dat$matched_climate[row] <- NA
    }
    return(coord.dat)

}

# Filter out rows where the matched climate data is NA
# This step removes any coordinate points that did not have valid climate data matches.
coord.dat <- filter(coord.dat, is.na(matched_climate) == FALSE)

  } else if (format == "multi.array"){
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

    # then, use key elements of cGENIE.data to extract the same data format from the array
    var.arr <- input
    # Extract general variables
    lat <- grid.dat$lat            # Latitude (degrees north)
    lat.edges <- grid.dat$lat.edges # Latitude edges
    lon <- grid.dat$lon            # Longitude (degrees east)
    lon.edges <- grid.dat$lon.edges # Longitude edges

    # Repeat the first 6 columns for each ecotype
    ecotypes <- 1:1000  # Assuming 1000 ecotypes, adjust as necessary
    expanded_df <- do.call(rbind, lapply(ecotypes, function(ecotype) {
        cbind(
            rep(lon, times = length(lat), each = 1),
            rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
            rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
            rep(lat, times = 1, each = length(lon)),
            rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
            rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
            as.data.frame(melt(var.arr[,, depth.level, ecotype]))$value,
            ecotype
        )
    }))

    expanded_df <- as.data.frame(expanded_df)

    names(expanded_df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var", "ecotype")

    # Filter the data to ensure valid lat-lon ranges
    expanded_df <- expanded_df %>%
        filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

    # Handle cells at the extremes (-180 and 180 longitude)
    expanded_df$lon.range <- abs(expanded_df$lon.min - expanded_df$lon.max)
    expanded_df$lon.min[expanded_df$lon.range > 180 & abs(expanded_df$lon.min) == 180] <- -expanded_df$lon.min[expanded_df$lon.range > 180 & abs(expanded_df$lon.min) == 180]
    expanded_df$lon.max[expanded_df$lon.range > 180 & abs(expanded_df$lon.max) == 180] <- -expanded_df$lon.max[expanded_df$lon.range > 180 & abs(expanded_df$lon.max) == 180]

    clim.dat <- expanded_df

    # Omit NAs in the var value for climate data file
# This step filters out any rows in the climate data where the specified variable has NA values.
# This ensures that only valid data points are used i§n the matching process.
clim.dat <- filter(clim.dat, !is.na(var))
# Remove any NA paleocoordinates
# This step filters out any rows in the coordinate data where the latitude or longitude values are NA.
# This ensures that only valid coordinates are used in the matching process.
coord.dat <- filter(coord.dat, !is.na(!!sym(lng.name)) & !is.na(!!sym(lat.name)))

# Initialize a column for matched climate data
# This step adds a new column to the coordinate data frame to store the matched climate data.
# Initially, all values in this column are set to NA.
coord.dat$matched_climate <- NA

# Load the progress package
library(progress)

# Use vectorized operations to speed up the matching process

# Find the mid-point of the nearest latitudinal grid cell for each occurrence
coord.dat$lat.bin.mid <- sapply(coord.dat[[lat.name]], function(lat) grid.dat$lat[which.min(abs(lat - grid.dat$lat))])
# Identify all the cells in the climate model that have the same latitude as the data point
lat.mid.opts <- clim.dat %>%
    filter(lat.mid %in% coord.dat$lat.bin.mid)

# Create a function to find the nearest longitude bin and matched climate data
find_lon_bin_and_climate <- function(lng, lat_bin_mid) {
    lat_mid_opts <- lat.mid.opts %>%
        filter(lat.mid == lat_bin_mid)

    if (nrow(lat_mid_opts) > 0 & min(abs(lng - lat.mid.opts$lon.mid)) < 10) {
        lon_bin_mid <- lat_mid_opts$lon.mid[which.min(abs(lng - lat_mid_opts$lon.mid))]
        matched_climate <- lat.mid.opts$var[lat.mid.opts$lat.mid == lat_bin_mid & lat.mid.opts$lon.mid == lon_bin_mid]
        ecotype <- lat.mid.opts$ecotype[lat.mid.opts$lat.mid == lat_bin_mid & lat.mid.opts$lon.mid == lon_bin_mid]
    } else {
        lon_bin_mid <- NA
        matched_climate <- NA
        ecotype <- NA
    }

    return(list(lon_bin_mid = lon_bin_mid, matched_climate = matched_climate, ecotype = ecotype))
}

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Matching climate data [:bar] :percent in :elapsed",
  total = nrow(coord.dat), clear = FALSE, width = 60
)

# Apply the function to each row in the coordinate data frame with progress bar
results <- mapply(function(lng, lat_bin_mid) {
  pb$tick()
  find_lon_bin_and_climate(lng, lat_bin_mid)
}, coord.dat[[lng.name]], coord.dat$lat.bin.mid, SIMPLIFY = FALSE)

# Initialize progress bar for extracting results
pb_extract <- progress_bar$new(
    format = "  Extracting results [:bar] :percent in :elapsed",
    total = length(results), clear = FALSE, width = 60
)

# Extract the results and expand the coordinate data frame
expanded_results <- do.call(rbind, lapply(1:length(results), function(i) {
    pb_extract$tick()
    res <- results[[i]]
    if (length(res$matched_climate) == 1000 && all(!is.na(res$matched_climate))) {
        data.frame(
            coord.dat[rep(i, length(res$matched_climate)), ],
            lon.bin.mid = res$lon_bin_mid,
            matched_climate = res$matched_climate,
            ecotype = res$ecotype
        )
    } else {
        NULL
    }
}))

return(expanded_results)

}

}
