require(AquaEnv)   # for gauge_p




# calculates pressure corrected pO2
###################################
# O2: umol/kg  oxygen concentration
#  S:          salinity
#  t: degC     temperature
#  d: m        depth
#######
# unit: matm
#######
pO2 <- function(O2, S, t, d, lat=30)
  {
    p   <- press(d=d, lat=lat)
    pO2 <- (p_corr_pp(pp=pO2_(O2=O2, t=t, S=S, p=p), p=p, t=t))/1000
    attr(pO2, "unit") <- "matm"
    return(pO2)
  }



# calculates [O2] from pressure corrected pO2
##############################################
# pO2: matm     pressure corrected oxygen partial pressure
#  S:           salinity
#  t:  degC     temperature
#  d:  m        depth
#######
# unit: matm
#######
O2 <- function(pO2, S, t, d, lat=30)
  {
    p   <- press(d=d, lat=lat)
    O2 <- O2_(pO2=rev_p_corr_pp(pp=pO2*1000, p=p, t=t), t=t, S=S, p=p)  
    return(O2)
  }







#######################################################################################################################
#######################################################################################################################

# wrapper for function gauge_p to make it suitable for vectors
##############################################################
# d:   m     depth
# lat: deg   latitude
#####
# unit: bar
#####
press <- function(d, lat)
  {
    press <- c()
    for (i in 1:length(d))
      {
        press <- c(press, gauge_p(d=d[[i]], lat=lat[[i]]))
      }
    return(press)
  }



# calculates seawater density (in kg/m3) from temperature (in degrees centigrade) and salinity
##############################################################################################
# references: Millero1981, DOE1994
seadensity <- function(S,                    # salinity S in practical salinity units (i.e. no unit)  
                       t)                    # temperature in degrees centigrade

  {
    t2 <- t^2
    t3 <- t2 * t
    t4 <- t3 * t
    t5 <- t4 * t
            
    A <- 8.24493e-1    - 4.0899e-3*t + 7.6438e-5*t2 - 8.2467e-7*t3 + 5.3875e-9*t4
    B <- -5.72466e-3 + 1.0227e-4*t - 1.6546e-6*t2
    C <- 4.8314e-4
    
    densityWater <- 999.842594 + 6.793952e-2*t - 9.095290e-3*t2 + 1.001685e-4*t3 - 1.120083e-6*t4 + 6.536332e-9*t5
        
    return(densityWater + A*S + B*S*sqrt(S) + C*S^2)  # seawater density in kg/m        
  }




# density anomaly (density differential to "standard pure water @ 1 bar": 1000 kg / m^3)
########################################################################################
# unit: kg/m3
######
sigma <- function(S,t,p)
  {
    sigma <- seadensity(S=S, t=t) - 1000
    attr(sigma, "unit") <- "kg/m3"
    return(sigma)
  }


# adiabatic temperature gradient
################################
# Ref: BRYDEN,H., 1973, DEEP-SEA RES. 20: 401-408. (adapted from visual basic script Ed Peltzer)
######
# S:  salinity            ("PSS-78")
# t:  temperature         (deg C)
# p:  applied pressure    (bar)
######
# unit: degc/bar
######
# checkvalue: delta_adiabat(40, 40, 1000) = 0.003255976 degC/bar
######
delta_adiabat <- function(S, t, p)
  {
    DS <- S - 35    
    p  <- p*10                            # convert bar to dbar

    delta_adiabat <- ((((-2.1687e-16*t + 1.8676e-14)*t - 4.6206e-13)*p + ((2.7759e-12*t - 1.1351e-10)*DS
                     +((-5.4481e-14*t + 8.733e-12)*t - 6.7795e-10)*t + 1.8741e-8))*p
                     +(-4.2393e-8*t + 1.8932e-6)*DS+((6.6228e-10*t - 6.836e-8)*t + 8.5258e-6)*t + 3.5803e-5)

    delta_adiabat <- delta_adiabat*10     # convert /dbar to /bar
    
    attr(delta_adiabat, "unit") <- "degC/bar"
    return(delta_adiabat)
  }


# potential temperature theta (according to reference p=0 (i.e. water surface @ 1 atm))
#######################################################################################
# adapted from visual basic script Ed Peltzer (Runge-Kutta 4-th order integration)
######
# S:  salinity            ("PSS-78")
# t:  temperature         (deg C)
# p:  applied pressure    (bar)
######
# unit: degC
######
# checkvalue: theta(40, 40, 1000) = 36.89073 degC
######
theta <- function(S, t, p)
  {
    PR <- 0                          # reference pressure
    H  <- PR - p                     # pressure differential

    tpar  <- c(0.5, 0.29289322,   1.707106781, 0)
    qpar1 <- c(1,   0.58578644,   3.414213562, 0)
    qpar2 <- c(0,   0.121320344, -4.121320344, 1)
    
    Q <- 0; XK <- 0 
    for (i in 1:4)
      {
        XK <- H*delta_adiabat(S, t, p)

        t  <- t + tpar[[i]]*(XK - Q)
        Q  <- qpar1[[i]]*XK + qpar2[[i]]*Q
          
        if (!(i==2)) {p  <- p + 0.5*H}
      }
    theta = t + (XK - 2*Q)/6
 
    attr(theta, "unit") <- "degC"
    return(theta)
  }


# potential density anomaly sigma-theta
#######################################
# S:  salinity            ("PSS-78")
# t:  temperature         (deg C)
# p:  applied pressure    (bar)
######
# unit: kg/m3
######
# check value (from Ed Peltzers pHcalc_mars_03.xls file): sigmatheta(34.3970, 4.620, 88.52) = 27.2479
######
sigmatheta <- function(S, t, p)
  {
    return(sigma(S, theta(S,t,p), 0))
  }



# hydrostatic pressure correction of gas partial pressure values
#################################################################
# Reference: Enns et al, 1964, eq 3
#####
# pp: gas partial pressure value       (uatm) or any other unit
# p:  hydrostatic (applied) pressure   (bar)
# t:  temperature                      (deg C)
# species: "O2", "CO2", "N2", "Ar", or "He"
######
# unit: uatm (or other unit, same as pp)
######
# "check" value (table first row): p_corr_pp(734.5, 102*1.01325, 20) [1] 839.3984 (839.5)
######
p_corr_pp <- function(pp, p, t, species="O2")
  {
    nu <- switch(species,
                 O2 =mean(31.9, 31.9, 32.2, 32.3, 32, 31.7),      # cm^3/mol (molar volume of O2 here)
                 CO2=mean(35.3, 34.4),
                 N2 =mean(33.2, 33.2, 33.5),
                 Ar =mean(32.3, 32.1),
                 He =mean(29.7, 29.7))
    R  <- 8.314472  # J/(mol*K) = Nm/(mol*K) = kg*m^2/s^2/(mol*K)
    P1 <- 0         # atm = 1.01325 bar = 1.01325*10^5 N/m^2
    T  <- t+273.15  # K
    p  <- p/1.01325 # atm = 1.01325 bar = 1.01325*10^5 N/m^2

    return(exp(-(nu*(P1-p)/(R*T*10)))*pp)
  }


# REVERSAL of the hydrostatic pressure correction of gas partial pressure values
#################################################################################
# Reference: Enns et al, 1964, eq 3
#####
# pp: gas partial pressure value       (uatm) or any other unit
# p:  hydrostatic (applied) pressure   (bar)
# t:  temperature                      (deg C)
# species: "O2", "CO2", "N2", "Ar", or "He"
######
# unit: uatm (or other unit, same as pp)
######
# "check" value: rev_p_corr_pp(p_corr_pp(734.5, 102*1.01325, 20), 102*1.01325, 20) [1] 734.5
######
rev_p_corr_pp <- function(pp, p, t, species="O2")
  {
    nu <- switch(species,
                 O2 =mean(31.9, 31.9, 32.2, 32.3, 32, 31.7),      # cm^3/mol (molar volume of O2 here)
                 CO2=mean(35.3, 34.4),
                 N2 =mean(33.2, 33.2, 33.5),
                 Ar =mean(32.3, 32.1),
                 He =mean(29.7, 29.7))
    R  <- 8.314472  # J/(mol*K) = Nm/(mol*K) = kg*m^2/s^2/(mol*K)
    P1 <- 0         # atm = 1.01325 bar = 1.01325*10^5 N/m^2
    T  <- t+273.15  # K
    p  <- p/1.01325 # atm = 1.01325 bar = 1.01325*10^5 N/m^2

    return(exp((nu*(P1-p)/(R*T*10)))*pp)
  }



# Oxygen Saturation concentration
#################################
# Reference: Garcia & Gordon, 1992
######
# S:  salinity
# t:  temperature (deg C)
######
# unit: umolO2/kg(soln)
######
# R version of Ed Peltzer's Matlab version: tested there
######
O2sat_new <- function(S, t)
  {
     #'CALCULATE OXYGEN CONCENTRATION AT SATURATION
     #'Source:  Oxygen solubility in seawater: Better fitting equations.
     #'          Garcia & Gordon (1992) L&O 37: 1307-1312.
     #'Input:       S = Salinity (pss-78)
     #'             T = Temp (deg C)
     #'Output:      Oxygen saturation at one atmosphere (umol/kg).

     #'DEFINE CONSTANTS, ETC FOR SATURATION CALCULATION

     #' The constants used are for units of umol O2/kg.
    T <- t
    
    A0 = 5.80871
    A1 = 3.20291
    A2 = 4.17887
    A3 = 5.10006
    A4 = -0.0986643
    A5 = 3.80369
    
    B0 = -0.00701577
    B1 = -0.00770028
    B2 = -0.0113864
    B3 = -0.00951519
    
    C0 = -0.000000275915
    
    #'CALCULATE Ts FROM T (deg C):

    TS = log((298.15 - T) / (273.15 + T))
  
    #'CALCULATE O2 SATURATION in umol/kg:

    A = ((((A5 * TS + A4) * TS + A3) * TS + A2) * TS + A1) * TS + A0
    B = ((B3 * TS + B2) * TS + B1) * TS + B0
    O2sat_new = exp(A + S * (B + S * C0))
  }



# Saturation Vapor pressure of Water
####################################
# Reference: Weiss & Price 1980 in Zeebe & Wolf-Gladrow (2001), p. 281
#### 
# S: salinity      (pss)
# t: temperature   (deg C)
####
# unit: atm
####
# checked by comparison with Ed's Matlab Scripts (for the 2010 Flyer Cruise)
####
sat_vap_press <- function(S, t)
  {
    T = t + 273.15
    return(exp(24.4543 - (6745.09/T) - 4.8489*log(T/100) - 0.000544*S))
  }


# calculate the (non pressure corrected) pO2 from [O2]
#####################################################
# O2: oxygen concentration    (umol/kg-soln)
# t:  temperature             (deg C)
# S:  salinity
# p:  hydrostatic pressure    (bar)
####
# unit: uatm
####
# checked by comparison with Ed's Matlab scripts (for the 2010 Flyer cruise)
####
pO2_ <- function(O2, t, S, p)
  {
    thet    <- theta(S=S, t=t, p=p)      # deg C
    O2_sat  <- O2sat_new(S=S, t=thet)    # umolO2/kg-soln
    xO2air  <- 0.20946                   # mole fraction: dimensionless
    
    pO2  <- (O2/O2_sat)*(xO2air*(1-sat_vap_press(S=S, t=thet))*1e6)   # at surface: total pressure = 1 atm 
    attr(pO2, "unit") <- "uatm"
    
    return(pO2)
  }


# calculate the [O2] from (the non pressure corrected) pO2
##########################################################
# pO2: oxygen partial pressure (uatm)
# t:   temperature             (deg C)
# S:   salinity
# p:   hydrostatic pressure    (bar)
####
# unit: umolO2/kg-soln
####
# checked by comparison with Ed's Matlab scripts (for the 2010 Flyer cruise)
####
O2_ <- function(pO2, t, S, p)
  {
    thet    <- theta(S=S, t=t, p=p)    # deg C
    O2_sat  <- O2sat_new(S=S, t=thet)  # umolO2/kg-soln
    xO2air  <- 0.20946                 # mole fraction: dimensionless
    
    O2 <- (pO2/(xO2air*(1-sat_vap_press(S=S, t=thet))*1e6))*O2_sat 
    attr(O2, "unit") <- "umolO2/kg-soln"
    
    return(O2)
  }


# fugacity coefficient for O2
#################################
# Reference: based on: Zeebe 2001, p. 283 (with second virial coefficient for O2 from Atkins 1996)
#################################
# t     temperature in deg C
# p     hydrostatic pressure in bar
#################################
# unit: -
########
fugacity_coeff_O2 <- function(t, p)
  {
    B <- -22e-6           #m^3/mol  SECOND virial coefficient for O2 at 273K (Atkins 1996)
    R <- 8.314472         #J/(mol*K) ideal gas constant = Nm/(mol*K)

    p <- (p+1.01325)*1e5  #Pa       total pressure
    T <- t+273.15         #K        absolute temperature

    f <- exp((B*p)/(R*T))

    return(f)
  }