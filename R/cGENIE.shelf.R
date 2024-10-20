#' Summarize Shelf Environments from cGENIE .nc Files
#'
#' This function summarizes the shelf environments from cGENIE .nc files.
#' The default shelf is the same as that used in Stockey et al. 2021 PNAS and Pohl et al. 2023 Sci Adv,
#' i.e., the top 3 layers of cells directly adjacent to land. The function can be customized to alter
#' the vertical number of cells and the horizontal distance from land.
#'
#' @param nc.sum A list containing the NetCDF summary data, including variables such as `var`, `time`, `lon`, `lat`, and `depth`.
#' @param shelf.depth An integer specifying the number of vertical layers to consider as the shelf. Default is 3.
#' @param array.only A logical value indicating whether to return only the array of the variable for shelf synthesis. Default is TRUE.
#' @param surf A logical value indicating whether to include the surface layer in the shelf. Default is FALSE.
#' @param time.step A character or integer specifying the time step to use. Default is "default", which uses the length of `nc.sum$time`.
#' @return An array summarizing the shelf environments.
#' @examples
#' \dontrun{
#' nc.sum <- list(var = array(data = NA, dim = c(10, 10, 10, 10)), time = 1:10, lon = 1:10, lat = 1:10, depth = 1:10)
#' result <- cGENIE.shelf(nc.sum)
#' }
#' @export
cGENIE.shelf <- function(nc.sum, shelf.depth = 3, array.only = TRUE, surf = FALSE, time.step = "default") {
  # -------------------------------------------------------------------------------------------------------
  # Default shelf is the same as that used in Stockey et al. 2021 PNAS and Pohl et al. 2023 Sci Adv
  # i.e., top 3 layers of cells directly adjacent to land
  # Easy to alter vertical number of cells
  # Could update function to be able to alter horizontal distance from land
  # -------------------------------------------------------------------------------------------------------
  # By default, only returns array of variable we are generating shelf synthesis of (for taking means, etc.)
  # If wanting to plot shelf, set array.only = FALSE to get spatial NetCDF variables back
  # -------------------------------------------------------------------------------------------------------

  # Print a message to indicate the function is running
  print("Function cGENIE.shelf is running")

  # Extract the variable array from the NetCDF summary data
  var <- nc.sum$var

  # Determine the time step to use
  if (time.step == "default") {
    time <- length(nc.sum$time)
  } else {
    time <- time.step
    print(paste("Time step is", time))
  }

  # Initialize the shelf array with dimensions based on longitude, latitude, and depth
  shelf.array <- array(dim = c(length(nc.sum$lon), length(nc.sum$lat), length(nc.sum$depth)))

  # Determine the depth vector based on whether the surface layer is included
  depth.vec <- if (surf) 1:shelf.depth else 2:shelf.depth

  # Loop through each longitude, latitude, and depth to populate the shelf array
  for (lon in 1:length(nc.sum$lon)) {
    for (lat in 1:length(nc.sum$lat)) {
      for (depth in depth.vec) {
        # Check if the current cell is on land
        if (is.na(var[lon, lat, depth, time])) {
          shelf.array[lon, lat, depth] <- NA
        } else {
          # Add your processing logic here
        }
      }
    }
  }

  # Return the shelf array
  return(shelf.array)
}