#' HADCM3.m2.to.global
#'
#' This function upscales per area estimates of variables (e.g., triffid outputs) to global values using data from HADCM3 model outputs.
#'
#' @param var A character string specifying the variable to be extracted from the NetCDF file.
#' @param file A character string specifying the file name (without extension) to be read from the experiment directory.
#' @param experiment A character string specifying the path to the experiment directory.
#' @param depth.level An integer specifying the depth level to be used for 3D data. Default is 1.
#' @param dims An integer specifying the dimensions of the NetCDF data (2 or 3). Default is 2.
#' @param unit.factor A numeric value to scale the variable. Default is 1.
#' @param time.present A logical value indicating if time data is present in the NetCDF file. Default is FALSE.
#'
#' @return A numeric value representing the total global value of the specified variable.
#'
#' @details
#' The function reads NetCDF files using the RNetCDF package and extracts latitude, longitude, and optionally depth and time data. It then processes the data to create a dataframe with the specified variable and its corresponding spatial coordinates. The function calculates the area of each grid cell and multiplies it by the variable value to obtain the total global value.
#'
#' @note
#' - The function assumes that the NetCDF files follow a specific structure and naming convention.
#' - The function includes several checks and adjustments for the HADCM3 grid geometry.
#' - The function uses various R packages for data manipulation and spatial calculations, including dplyr, sf, sp, ggspatial, reshape2, ggplot2, pals, viridis, and geosphere.
#'
#'
#' @import RNetCDF dplyr sp reshape2 geosphere
#' @export

HADCM3.m2.to.global <- function(var, file, experiment,
                        depth.level = 1,
                        dims = 2,
                       unit.factor = 1,
                       time.present = FALSE){

  # Load necessary libraries for the function
  library(RNetCDF)  # For reading NetCDF files
  library(dplyr)    # For data manipulation
  library(sp)       # For spatial data handling
  library(reshape2) # For reshaping data
  library(geosphere) # For geospatial calculations

  nc <- open.nc(paste0(experiment, file, ".nc"))

  # Extract general variables
  # NOTE - these may not precisely represent the HADCM3 grid
  # fudged slightly for ease of plotting from the xxx and xxx_1 variables.
  # worth checking with HADCM3 users to be doubly sure
  # this should be kept separate from variable matching scripts with point data such as PBDB and therefore be functionally fine
  lat <- var.get.nc(nc, "latitude") # units: degrees north
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "longitude") # units: degrees east
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  if(dims == 3){
  depth <- var.get.nc(nc, "depth_1") # units: metres
  depth.edges <- c(0, var.get.nc(nc, "depth"), (depth[length(depth)]+307.5)) # units: metres # NOTE - the bounding of the bottom box is fudged but seems to be reasonably fudged. All deep ocean cells ~307.5*2m deep
  }
  if(time.present == TRUE){
  time <- var.get.nc(nc, "t") # units: year mid-point - NOTE, if want to use this then would need to update time name.
  # note that not all of these general variables will be available for fields_biogem_2d (address later)
}
  # Extract named variable
  var.arr <- var.get.nc(nc, var)

  # NOTE - this is what i have done with cGENIE models.
  # Is this the best way to deal with here,
  # or just another way of translating to a nice grid?
  # maybe for plotting either is kind of fine.
  # but definitely would need to be fixed for point data matching.
  # deal with weird lon coordinates if present
  # does lon live between -180 and 180? and are there a normal 36 increments? (is the second one important?)
  # if(mean(between(lon, -180, 180)) < 1){
  #   add_on <- -(lon.edges[1] + 180)
  #   lon.edges <- lon.edges + add_on
  #   lon <- lon + add_on
  # }

  # amend HADCM3 grid to project on 0 degs
  if(mean(between(lon, -180, 180)) < 1){
    lon.edges[lon.edges > 180] <- lon.edges[lon.edges >180] - 360
    lon[lon >180] <- lon[lon >180] -360
  }


  if(dims == 3){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      #as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))
      as.data.frame(melt(var.arr[,, depth.level]))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
  }
  if(dims == 2){
    # generate dataframe of 2d genie slice from 3d genie array
    df <- as.data.frame(cbind(
      rep(lon, times = length(lat), each = 1),
      rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
      rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
      rep(lat, times = 1, each = length(lon)),
      rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
      rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
      #as.data.frame(melt(var.arr[,, time.step]))$value))
      as.data.frame(melt(var.arr))$value))

    names(df) <- c("lon.mid",
                   "lon.min",
                   "lon.max",
                   "lat.mid",
                   "lat.min",
                   "lat.max",
                   "var"
    )
  }

  # eliminate cells outside of reasonable range
  df <- df %>%
    filter(lon.max <= 180,
           lon.min >= -180,
           lat.max <= 90,
           lat.min >= -90,
           lat.max >= -90, # ADDING THIS ON 20240408 AS FOR TRIFFIC FILES COUNTING SEEMS TO BE FLIPPED...
           lat.min <= 90
    )

  # NOTE - NOT SURE I HAVE A PERFECT UNDERSTANDING OF THE HADCM3 grid geometry but this should be pretty much correct as looks fine in map view
  # Sit down with BRIDGE group at somepoint?

  # SCRAPPED THIS FOR TOTAL CALCULATIONS...
  # # also eliminate cells that bridge left and right side of map (i.e. extreme -180ish and 180ish longitude)
  # df$lon.range <- abs(df$lon.min-df$lon.max)
  # df <- df %>%
  #   filter(lon.range < 180 #could just be greater than 4, but this will work for all model grids
  #   )


  # Initialize lists to store polygon objects and their names
  poly.list <- list()
  poly.names.list <- list()

  # Loop through each row of the dataframe to create polygons
  for(poly in 1:(nrow(df))){
    # Create a polygon for each grid cell using the min and max lat/lon values
    polygon.code <- Polygon(cbind(
      c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
      c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
    # Assign the polygon to a variable with a unique name
    assign(paste0("Polygon_", poly), polygon.code)

    # Create a Polygons object (a collection of Polygon objects) for each grid cell
    polygons.code <- Polygons(list(polygon.code), paste0("p",poly))
    # Assign the Polygons object to a variable with a unique name
    assign(paste0("Polygons_", poly), polygons.code)

    # Append the Polygons object to the list of polygons
    poly.list <- append(poly.list, polygons.code)
    # Append the name of the Polygons object to the list of names
    poly.names.list <- append(poly.names.list, paste0("p",poly))
  }

  # Create a SpatialPolygons object from the list of Polygons objects
  SpP <- SpatialPolygons(poly.list)

  # Calculate the area of each polygon in square meters
  area_m2 <- areaPolygon(SpP)

  # Add the calculated area as a new column to the dataframe
  df <- cbind(df, area_m2)

  # Calculate the total value for each grid cell by multiplying the variable value by the area
  df$TotalCellVal <- df$var * df$area_m2

  # Sum the total values of all grid cells to get the global value
  TotalVal <- sum(df$TotalCellVal, na.rm = TRUE)

  # Return the total global value
  return(TotalVal)
}


