#' Extract 2D Data Fields from 3D or 2D netCDF Files
#'
#' This function extracts 2D latitude-longitude data fields from 3D or 2D netCDF (.nc) files.
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param depth.level Depth level to extract the data from. Set to NULL by default.
#' @param dims Number of dimensions in the netCDF file. Defaults to 3.
#' @param year Year to extract data for (default is "default", meaning the last time point).
#' @param model The model type; defaults to "biogem".
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr %>%
#' @importFrom reshape2 melt
#' @export

cGENIE.data <- function(var, experiment,
                        depth.level = NULL,
                        dims = 3,            # Set default dims to 3
                        year = "default",
                        model = "biogem") {
# Define the prefix based on the selected model
if (model == "biogem") {
prefix <- "/biogem/fields_biogem_"
}

# Open the netCDF file based on the dimensions
nc <- RNetCDF::open.nc(paste0(experiment, prefix, dims, "d", ".nc"))

# Extract general variables
lat <- RNetCDF::var.get.nc(nc, "lat")            # Latitude (degrees north)
lat.edges <- RNetCDF::var.get.nc(nc, "lat_edges")
lon <- RNetCDF::var.get.nc(nc, "lon")            # Longitude (degrees east)
lon.edges <- RNetCDF::var.get.nc(nc, "lon_edges")

# Determine the time step to extract
if (year == "default") {
time.step <- dim(RNetCDF::var.get.nc(nc, "time"))[1]  # Last time point
} else {
time.step <- which(RNetCDF::var.get.nc(nc, "time") == year)
}

                          # Extract the variable array
                          if (dims == 3) {
                            var.arr <- RNetCDF::var.get.nc(nc, var)
                          } else if (dims == 2) {
                            var.arr <- RNetCDF::var.get.nc(nc, var)
                          }

                          # Create data frame for 3D array
                          if (dims == 3) {
                            df <- as.data.frame(cbind(
                              rep(lon, times = length(lat), each = 1),
                              rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat), each = 1),
                              rep(lon.edges[2:length(lon.edges)], times = length(lat), each = 1),
                              rep(lat, times = 1, each = length(lon)),
                              rep(lat.edges[1:(length(lat.edges) - 1)], times = 1, each = length(lon)),
                              rep(lat.edges[2:length(lat.edges)], times = 1, each = length(lon)),
                              as.data.frame(reshape2::melt(var.arr[, , depth.level, time.step]))$value
                            ))
                            names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
                          }

                          # Create data frame for 2D array
                          if (dims == 2) {
                            if (var == "grid_topo") {
                              df <- as.data.frame(cbind(
                                rep(lon, times = length(lat)),
                                rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat)),
                                rep(lon.edges[2:length(lon.edges)], times = length(lat)),
                                rep(lat, each = length(lon)),
                                rep(lat.edges[1:(length(lat.edges) - 1)], each = length(lon)),
                                rep(lat.edges[2:length(lat.edges)], each = length(lon)),
                                as.data.frame(reshape2::melt(var.arr))$value
                              ))
                              names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
                            } else if (var == "phys_psi") {
                              df <- as.data.frame(cbind(
                                rep(lon + 5, times = length(lat.edges)),
                                rep(lon.edges[1:(length(lon.edges) - 1)] + 5, times = length(lat.edges)),
                                rep(lon.edges[2:length(lon.edges)] + 5, times = length(lat.edges)),
                                rep(lat.edges, each = length(lon)),
                                rep(lat.edges, each = length(lon)),
                                rep(lat.edges, each = length(lon)),
                                as.data.frame(reshape2::melt(var.arr))$value
                              ))
                              names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
                            } else {
                              df <- as.data.frame(cbind(
                                rep(lon, times = length(lat)),
                                rep(lon.edges[1:(length(lon.edges) - 1)], times = length(lat)),
                                rep(lon.edges[2:length(lon.edges)], times = length(lat)),
                                rep(lat, each = length(lon)),
                                rep(lat.edges[1:(length(lat.edges) - 1)], each = length(lon)),
                                rep(lat.edges[2:length(lat.edges)], each = length(lon)),
                                as.data.frame(reshape2::melt(var.arr[, , time.step]))$value
                              ))
                              names(df) <- c("lon.mid", "lon.min", "lon.max", "lat.mid", "lat.min", "lat.max", "var")
                            }
                          }

                          if (min(lon.edges) == -180) {
                            # No action needed
                          } else if (min(lon.edges) == -260) {
                            print("correcting for longitude offset, beta version...")

                            df$offset[df$lon.min < -180] <- -270 - df$lon.min[df$lon.min < -180]
                            df$lon.min[!is.na(df$offset)] <- 90 + abs(df$offset[!is.na(df$offset)])
                            df$lon.mid[!is.na(df$offset)] <- 95 + abs(df$offset[!is.na(df$offset)])
                            df$lon.max[!is.na(df$offset)] <- 100 + abs(df$offset[!is.na(df$offset)])
                          } else {
                            print("Longitude range is not -180 to 180 or -260 to 90")
                          }

                          # Filter the data to ensure valid lat-lon ranges
                          df <- df %>%
                            dplyr::filter(lon.max <= 180, lon.min >= -180, lat.max <= 90, lat.min >= -90)

                          # Handle cells at the extremes (-180 and 180 longitude)
                          df$lon.range <- abs(df$lon.min - df$lon.max)
                          df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180] <- -df$lon.min[df$lon.range > 180 & abs(df$lon.min) == 180]
                          df$lon.max[df$lon.range > 180 & abs(df$lon.max) == 180] <- -df$lon.max[df$lon.range > 180 & abs[df$lon.max] == 180]

                          return(df)
                        }
