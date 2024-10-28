#' cGENIE.MI.array.R
#'
#' This function processes cGENIE model output to calculate habitat viability and ecotype viability based on metabolic index models.
#'
#' @param experiment Character. The path to the experiment directory containing cGENIE output files.
#' @param year Character or numeric. The year to process. If "default", the final time step is used.
#' @param n_ecotypes Integer. The number of ecophysiotypes (ecotypes) to model. Default is 1000.
#' @param seed_number Integer. The seed number for random number generation. Default is 1993.
#' @param format Character. The format of the output. Either "habitat_viability" or "ecotype_viability". Default is "habitat_viability".
#'
#' @return Depending on the format parameter, returns either a 3D array of habitat viability percentages or a 4D array of ecotype viability.
#'
#' @details
#' This function reads cGENIE model output and calculates habitat viability and ecotype viability using metabolic index models. It sources functions from Hofmann et al. (2011) and uses parameter values from Penn et al. (2018). The function processes the data to generate arrays of partial pressure of oxygen (pO2) and temperature in Kelvin, and then calculates the viability of each ecotype in each model cell.
#'
#' @import AquaEnv
#' @import dplyr
#' @import ncdf4
#' @import reshape2
#' @import RNetCDF
#' @export
# Note – If you want to only use shelf environments in this script, need to generate array here, then run array through cGENIE.shelf
#
cGENIE.MI.array.local <- function(
    experiment,
    year = "default",
    n_ecotypes = 1000,
    seed_number = 1993,
    format = "habitat_viability"
) {

library(AquaEnv)
library(dplyr)
library(ncdf4)
library(reshape2)
library(RNetCDF)

# Set the time step to the final value if year is "default"
if (year == "default") {
time.step <- length(time)
} else {
time.step <- year
}

# Source functions from Hofmann et al. 2011 Deep Sea Research (set directory if necessary)
source("~/PaleoClimR/sources/Hofmann_functions.R")

################################
## Define parameters for metabolic index model
################################

# Set number of ecophysiotypes in model - abbreviated to 'ecotype'
# throughout this script for concision
init.ecotypes <- n_ecotypes

# set seed for random number generation
set.seed(seed_number)

# input Penn et al. 2018 parameter values
u_A <- 3.01 # log(atm ^-1)
sig_A <- 0.49 # log(atm ^-1)

u_C <- 1.10 # unitless
sig_C <- 0.42 # unitless

u_E <- 0.41 # eV
sig_E <- 0.29 # eV

# sample Ao values from PDF (log-normal distribution)
A0.xxx <- rlnorm(init.ecotypes, meanlog = u_A, sdlog = sig_A)

# sample Eo values from PDF (normal distribution)
E0.xxx <- rnorm(init.ecotypes, mean = u_E, sd = sig_E)

# sample phi crit values from PDF (log-normal distribution)
phi_crit.xxx <- rlnorm(init.ecotypes, meanlog = u_C, sdlog = sig_C)

# Set Penn et al. 2018 constants
kB <- 8.61733326e-5
Tref <- 15+273.15

################################
## Read cGENIE output
################################

Nc = open.nc(paste0(experiment, "/biogem/fields_biogem_3d", ".nc"))

## =============================================================
## Extract annual means of key variables
## =============================================================
lat <- var.get.nc(Nc, "lat") # units: degrees north
lat.edges <- var.get.nc(Nc, "lat_edges")
lon <- var.get.nc(Nc, "lon") # units: degrees east
lon.edges <- var.get.nc(Nc, "lon_edges")
depth <- var.get.nc(Nc, "zt") # units: metres
depth.edges <- var.get.nc(Nc, "zt_edges") # units: metres
times <- var.get.nc(Nc, "time") # units: year mid-point
oxy <- var.get.nc(Nc, "ocn_O2") # units: mol kg-1
sal <- var.get.nc(Nc, "ocn_sal") # units: PSU
temp <- var.get.nc(Nc, "ocn_temp") # units: degrees C
topo <- var.get.nc(Nc, "grid_topo") # units: meters

# =============================================================
# No density output from cGENIE - so here we use seadensity(S,t) function from Hofmann et al 2011
# Seadensity takes S in PSU and t in degrees C so no change from cGENIE required
# =============================================================

## =============================================================
## set desired time bin
## =============================================================

# time <- as.numeric(length(time)) # (last of model simulation)
#time = which(times == genie_yr) # based on year provided in command line
time <- time.step
## =============================================================
## set desired dimensions of array
## =============================================================
xdim <- as.numeric(length(lon))
ydim <- as.numeric(length(lat))
tdim <- as.numeric(length(times))
zdim <- as.numeric(length(depth))

## =============================================================
## make array of pO2 - units are atm
## =============================================================
oxygen_umol_kg <- oxy*10^6  # convert to umol/kg from mol/kg

depth_array <- array(rep(depth, each=xdim*ydim), dim=c(xdim,ydim,zdim))
lat_array <- array(rep(lat, each = xdim, times = zdim), dim=c(xdim,ydim,zdim))
## pass oxygen, salinity, temp and depth to Hoffman et al. 2011's pO2 function
pO2_array <- pO2(O2=oxygen_umol_kg[,,,time], S=sal[,,,time], t=temp[,,,time], d=depth_array, lat=lat_array)/1000

## =============================================================
## Convert temp to Kelvin
## =============================================================

temp_K <- temp[,,,time]+273.15

## =============================================================
## Habitat viability model - calculate viability per cGENIE model cell
## =============================================================

# initiate array for habitat viability (only saved if NetCDF saving activated)
habitat_viability <- array(dim=c(xdim,ydim,zdim))
ecotype_viability <- array(dim=c(xdim,ydim,zdim,n_ecotypes))

# initiate data frame for viable ecotype summary (one per cGENIE simultion)
phi.crit.xxx.summary <-  data.frame(A0.xxx = double(), phi_crit.xxx=double(), temp_K=double(), pO2=double(), phi.xxx=double(), ecotype=double(), depth=double())

# loop through by cell (we are evaluating xxx physiological ecotypes per cell)
for(x in 1:xdim){
  for(y in 1:ydim){ # in published versions we excluded polar environments, here just want to return arrays
    z_counter = 1
    for(z in 1:zdim){
      pO2.cell <- pO2_array[x,y,z]
      temp.cell <- temp_K[x,y,z]

      if(is.na(pO2.cell) == TRUE | is.na(temp.cell) == TRUE){
        # Do nothing! If these are true - then this is land/crust!

      }else{
        # Calculate phi values for all xxx ecotypes (eqn. from Penn et al. 2018)
        phi.xxx <- A0.xxx * (pO2.cell/(exp((-E0.xxx/kB)*((1/temp.cell)-(1/Tref)))))

        # Combine relevant data into a dataframe
        phi.cell <- cbind(A0.xxx, phi_crit.xxx, rep(temp.cell, init.ecotypes), rep(pO2.cell, init.ecotypes), phi.xxx, seq(1:init.ecotypes), depth[z])

        # Name variables in cell ecotypes dataframe so that we can refer to them easily
        names(phi.cell) <- c("A0.xxx", "phi_crit.xxx", "temp_K", "pO2", "phi.xxx", "ecotype", "depth")

        # Filter the cell ecotypes dataframe to only include viable ecotypes (phi > phi_crit)
        phi.crit.xxx <- phi.cell %>%
                        as.data.frame()  %>%
                        filter(phi.xxx >= phi_crit.xxx)

        # populating ecoptype viability array
        ix = which(phi.xxx >= phi_crit.xxx)
        ecotype_viability[x,y,z_counter,ix] = 1
        nx = which(phi.xxx < phi_crit.xxx)
        ecotype_viability[x,y,z_counter,nx] = 0

        # Add habitat viability (percentage of total ecotypes that are viable in a given cell) to summary array (for building NetCDFs if so inclined)
        habitat_viability[x,y,z_counter] <- nrow(phi.crit.xxx)/init.ecotypes*100
      }
      z_counter = z_counter + 1
    }}}


if(format == "habitat_viability"){
    return(habitat_viability)
}else if(format == "ecotype_viability"){
    return(ecotype_viability)
}
}

