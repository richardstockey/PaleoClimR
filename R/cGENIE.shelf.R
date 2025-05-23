#' Summarize Shelf Environments from cGENIE .nc Files
#'
#' This function summarizes the shelf environments from cGENIE .nc files.
#' The default shelf is the same as that used in Stockey et al. 2021 PNAS and Pohl et al. 2023 Sci Adv,
#' i.e., the top 3 layers of cells directly adjacent to land. The function can be customized to alter
#' the vertical number of cells and the horizontal distance from land.
#'
#' @param input Input file, either a NetCDF summary (from cGENIE.nc.import) or an array of the variable.
#' @param shelf.depth An integer specifying the number of vertical layers to consider as the shelf. Default is 3.
#' @param array.only A logical value indicating whether to return only the array of the variable for shelf synthesis. Default is TRUE.
#' @param surf A logical value indicating whether to include the surface layer in the shelf. Default is FALSE.
#' @param time.step A character or integer specifying the time step to use. Default is "default", which uses the length of `input$time`.
#' @param format A character string specifying the format of the output. Default is "array", alternatively can use "nc.sum" or "multi.array".
#' @return An array summarizing the shelf environments.
#' @export
cGENIE.shelf <- function(input, format = "array", shelf.depth = 3, array.only = TRUE, surf = FALSE, time.step = "default") {
  # -------------------------------------------------------------------------------------------------------
  # Default shelf is the same as that used in Stockey et al. 2021 PNAS and Pohl et al. 2023 Sci Adv
  # i.e., top 3 layers of cells directly adjacent to land
  # Easy to alter vertical number of cells
  # Could update function to be able to alter horizontal distance from land
  # -------------------------------------------------------------------------------------------------------
  # By default, only returns array of variable we are generating shelf synthesis of (for taking means, etc.)
  # If wanting to plot shelf, set array.only = FALSE to get spatial NetCDF variables back
  # -------------------------------------------------------------------------------------------------------
  if (format == "nc.sum") {
    # Extract the variable array from the NetCDF summary data - generated by cGENIE.nc.import
    nc.sum <- input
    var <- nc.sum$var
    # Determine the time step to use
    if (time.step == "default") {
      time <- length(nc.sum$time)
    } else {
      time <- time.step
      base::print(base::paste("Time step is", time))
    }
  } else if (format == "array") {
    # Extract the variable array from the input
    var <- input
  } else if (format == "multi.array") {
    # Extract the variable array from the input
    var <- input
  }

  # Initialize the shelf array based on the format
  if (format == "nc.sum") {
    # Initialize the shelf array with dimensions based on longitude, latitude, and depth
    shelf.array <- array(dim = c(length(nc.sum$lon), length(nc.sum$lat), length(nc.sum$depth)))
  } else {
    # Initialize the shelf array with the same dimensions as var
    shelf.array <- array(dim = dim(var))
  }

  # Determine the depth vector based on whether the surface layer is included
  depth.vec <- if (surf) 1:shelf.depth else 2:shelf.depth
  if (format == "nc.sum") {
    for (lon in 1:length(nc.sum$lon)) {
      for (lat in 1:length(nc.sum$lat)) {
        for (depth in depth.vec) {
          if (is.na(var[lon, lat, depth, time]) == TRUE) { # if we're on land
            shelf.array[lon, lat, depth] <- NA
          } else {
            # CHECK if statements here
            if (lat == 36) {
              if (lon > 18) {
                var.N <- var[lon - 18, lat, depth, time]
              }
              if (lon < 18) {
                var.N <- var[36 + lon - 18, lat, depth, time]
              }
            } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.N <- var[lon, lat + 1, depth, time]
            }
            if (lat == 1) {
              if (lon > 18) {
                var.S <- var[lon - 18, lat, depth, time]
              }
              if (lon < 18) {
                var.S <- var[36 + lon - 18, lat, depth, time]
              }
            } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.S <- var[lon, lat - 1, depth, time]
            }
            if (lon == 36) {
              var.E <- var[1, lat, depth, time]
            } else { # if going east from 36, go to 1
              var.E <- var[lon + 1, lat, depth, time]
            }
            if (lon == 1) {
              var.W <- var[36, lat, depth, time]
            } else { # if going east from 1, go to 36
              var.W <- var[lon - 1, lat, depth, time]
            }

            if (is.na(var.N) == TRUE | is.na(var.S) == TRUE | is.na(var.E) == TRUE | is.na(var.W) == TRUE) {
              shelf.array[lon, lat, depth] <- var[lon, lat, depth, time]
            } else {
              shelf.array[lon, lat, depth] <- NA
            }
          }
        }
      }
    }
  } else if (format == "array") {
    for (lon in 1:dim(var)[1]) {
      for (lat in 1:dim(var)[2]) {
        for (depth in depth.vec) {
          if (is.na(var[lon, lat, depth]) == TRUE) { # if we're on land
            shelf.array[lon, lat, depth] <- NA
          } else {
            # CHECK if statements here
            if (lat == 36) {
              if (lon > 18) {
                var.N <- var[lon - 18, lat, depth]
              }
              if (lon < 18) {
                var.N <- var[36 + lon - 18, lat, depth]
              }
            } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.N <- var[lon, lat + 1, depth]
            }
            if (lat == 1) {
              if (lon > 18) {
                var.S <- var[lon - 18, lat, depth]
              }
              if (lon < 18) {
                var.S <- var[36 + lon - 18, lat, depth]
              }
            } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.S <- var[lon, lat - 1, depth]
            }
            if (lon == 36) {
              var.E <- var[1, lat, depth]
            } else { # if going east from 36, go to 1
              var.E <- var[lon + 1, lat, depth]
            }
            if (lon == 1) {
              var.W <- var[36, lat, depth]
            } else { # if going east from 1, go to 36
              var.W <- var[lon - 1, lat, depth]
            }

            if (is.na(var.N) == TRUE | is.na(var.S) == TRUE | is.na(var.E) == TRUE | is.na(var.W) == TRUE) {
              shelf.array[lon, lat, depth] <- var[lon, lat, depth]
            } else {
              shelf.array[lon, lat, depth] <- NA
            }
          }
        }
      }
    }
  }
  if (format == "multi.array") {

    for (lon in 1:dim(var)[1]) {
      for (lat in 1:dim(var)[2]) {
        for (depth in depth.vec) {
          for (var_dim in 1:dim(var)[4]) {
            if (is.na(var[lon, lat, depth, var_dim]) == TRUE) { # if we're on land
              shelf.array[lon, lat, depth, var_dim] <- NA
            } else {
              # CHECK if statements here
              if (lat == 36) {
                if (lon > 18) {
                  var.N <- var[lon - 18, lat, depth, var_dim]
                }
                if (lon < 18) {
                  var.N <- var[36 + lon - 18, lat, depth, var_dim]
                }
              } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
                var.N <- var[lon, lat + 1, depth, var_dim]
              }
              if (lat == 1) {
                if (lon > 18) {
                  var.S <- var[lon - 18, lat, depth, var_dim]
                }
                if (lon < 18) {
                  var.S <- var[36 + lon - 18, lat, depth, var_dim]
                }
              } else { # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
                var.S <- var[lon, lat - 1, depth, var_dim]
              }
              if (lon == 36) {
                var.E <- var[1, lat, depth, var_dim]
              } else { # if going east from 36, go to 1
                var.E <- var[lon + 1, lat, depth, var_dim]
              }
              if (lon == 1) {
                var.W <- var[36, lat, depth, var_dim]
              } else { # if going east from 1, go to 36
                var.W <- var[lon - 1, lat, depth, var_dim]
              }

              if (is.na(var.N) == TRUE | is.na(var.S) == TRUE | is.na(var.E) == TRUE | is.na(var.W) == TRUE) {
                shelf.array[lon, lat, depth, var_dim] <- var[lon, lat, depth, var_dim]
              } else {
                shelf.array[lon, lat, depth, var_dim] <- NA
              }
            }
          }
        }
      }
    }
  }

  # Return the shelf array
  return(shelf.array)
}
