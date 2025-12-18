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
cGENIE.data <- function(var,
                        experiment,
                        depth.level = NULL,
                        dims = 3,
                        year = "default",
                        model = "biogem") {

  nc.list <- cGENIE.get.nc(var = var,
                           experiment = experiment,
                           dims = dims,
                           model = model)

  # Select time step
  if (year == "default") {
    time.step <- dim(nc.list$time)[1]
  } else {
    time.step <- which(nc.list$time == year)
  }

  # build dataframe from extracted list
  nc.df <- nc.list.to.df(lat = nc.list$lat,
                         lat.edges = nc.list$lat.edges,
                         lon = nc.list$lon,
                         lon.edges = nc.list$lon.edges,
                         var = nc.list$var,
                         depth.level = depth.level,
                         dims = dims,
                         time.step = time.step
                           )
  return(nc.df)

}
