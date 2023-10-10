###################################################
# cGENIE.shelf.R
# Rich Stockey 20230922
# designed to summarise the shelf environments from cGENIE .nc files (imported using cGENIE.nc.import.R)
###################################################
# full comments to follow...

cGENIE.shelf <- function(nc.sum, shelf.depth = 3, array.only = TRUE){
  # -------------------------------------------------------------------------------------------------------
  # default shelf is the same as that used in Stockey et al. 2021 PNAS and Pohl et al. 2023 Sci Adv
  # i.e. top 3 layers of cells directly adjacent to land
  # easy to alter vertical number of cells
  # could update function to be able to alter horizontal distance from land
  # -------------------------------------------------------------------------------------------------------
  # by default only returns array of variable we are generating shelf synthesis of (for taking means, etc.)
  # if wanting to plot shelf, set array.only = FALSE to get spatial NetCDF variables back
  # -------------------------------------------------------------------------------------------------------

    var <- nc.sum$var
    time <- length(nc.sum$time)
    shelf.array <- array(dim=c(length(nc.sum$lon),length(nc.sum$lat),length(nc.sum$depth)))

    if(surf == TRUE){
      depth.vec <- 1:shelf.depth
    }
    if(surf == FALSE){
      depth.vec <- 2:shelf.depth
    }

    for(lon in 1:length(nc.sum$lon)){
      for(lat in 1:length(nc.sum$lat)){
        for(depth in depth.vec){

          if(is.na(var[lon, lat, depth, time]) == TRUE){ #if we're on land
            shelf.array[lon, lat, depth] <- NA
            #print("we're on land")
          } else{
            # CHECK if statements here
            if(lat == 36){
              if(lon > 18){
                var.N <- var[lon-18, lat, depth, time]
              }
              if(lon < 18){
                var.N <- var[36+lon-18, lat, depth, time]
              }

            } else{ # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.N <- var[lon, lat+1, depth, time]}
            if(lat == 1){
              if(lon > 18){
                var.S <- var[lon-18, lat, depth, time]
              }
              if(lon < 18){
                var.S <- var[36+lon-18, lat, depth, time]
              }
            } else{ # if go over pole, swap to opposite longitude and stay at same latitude (check logic)
              var.S <- var[lon, lat-1, depth, time]}
            if(lon == 36){
              var.E <- var[1, lat, depth, time]} else{ # if going east from 36, go to 1
                var.E <- var[lon+1, lat, depth, time]}
            if(lon == 1){
              var.W <- var[36, lat, depth, time]} else{ # if going east from 1, go to 36
                var.W <- var[lon-1, lat, depth, time]}

            if(is.na(var.N) == TRUE | is.na(var.S) == TRUE | is.na(var.E) == TRUE | is.na(var.W) == TRUE){
              shelf.array[lon, lat, depth] <- var[lon, lat, depth, time]
            } else{
              shelf.array[lon, lat, depth] <- NA
            }
          }
        }}}


  return(shelf.array)
}
