#' Extract Data from NetCDF Files
#'
#' This function extracts data from imported NetCDF (.nc) files, specifically designed for use with HADCM3 model outputs.
#'
#' @param var A character string specifying the name of the variable to extract from the NetCDF file.
#' @param file A character string indicating the name of the NetCDF file (without the .nc extension).
#' @param experiment A character string indicating the path to the HADCM3 experiment directory.
#' @param depth.level Depth level to extract the data from. Set to NULL by default.
#' @param dims An integer specifying the dimensions of the NetCDF file to read. Default is set to 3 for 3D data but can alternatively be set to 2. .
#' @param year Year to extract data for (default is "default", meaning the last or only time point (note this is slightly different to cGENIE.data because HADCM3 outputs more commonly have only one time point)).
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @export
HADCM3.data <- function(var,
                        file,
                        experiment,
                        depth.level = 1,
                        dims = 3,
                        year = "default"
                        ){

  nc.list <- HADCM3.get.nc(var = var,
                           file = file,
                           experiment = experiment,
                           dims = dims
                          )


  # Select time step
  if (year == "default") {
    # if there are multiple timesteps, this will be the last one
    # if there is only one (no) timestep[s] this will be NULL becase then it will be a 'random' number, which is then handled in nc.list.to.df()
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
