
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#getTscale
#
#calculates Tscale according to eqn 6 in Mahadevan et al, 2007
#INPUTS
#   T: 1xN array of observed temperatures
#   Tmin: minimum temp at which photosynthesis occurs
#   Tmax: maximum temp at which photosynthesis occurs
#   Topt: optimal temperature for photosynthesis
#OUTPUTS
#   Tscale: Tscale term in VPRM equation (eqn 12 in Mahadevan et al, 2007)
#
#twhilton, PSU, Dec 2007

getTscale <- function(T, Tmax, Tmin, Topt) {
  
  numer <- (T - Tmin) * (T - Tmax)
  denom <- ((T - Tmin) * (T - Tmax)) - (T - Topt)^2

  Tscale = numer / denom
  Tscale[which(Tscale < Tmin)] <- Tmin
  return(Tscale)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#getPscale
#
#calculates Pscale according to eqn 7 in Mahadevan et al, 2007
#INPUTS
#   LSWI: land surface water index
#   phen: MODIS phenology
#OUTPUTS
#   Pscale: Pscale term in VPRM equation (eqn 12 in Mahadevan et al, 2008)
#
#twhilton, PSU, Dec 2007

getPscale <- function( LSWI, phen ) {
  
  Pscale <- (1 + LSWI)/2
  Pscale[ phen == 'gmax' ] <- 1.0
  return( Pscale )

}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#getWscale
#
#calculates Wscale according to eqn 8 in Mahadevan et al, 2007
#INPUTS
#   LSWI: land surface water index
#   LSWI_max: maximum LSWI for site
#OUTPUTS
#   Wscale: Pscale term in VPRM equation (eqn 12 in Mahadevan et al, 2007)
#
#twhilton, PSU, Dec 2007

getWscale <- function(LSWI, LSWI_max) {
  Wscale <- (1 + LSWI) / (1 + LSWI_max)
  return(Wscale)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#getEVI
#
#calculates EVI according to eqn 2 in Mahadevan et al, 2007
#INPUTS
#   rho_nir: near-infrared (841-876 nm) band satellite-derived reflectance
#   rho_red: red band satellite-derived reflectance
#   rho_blue: blue band satellite-derived reflectance
#OUTPUTS
#   EVI: EVI term in VPRM equation (eqn 12 in Mahadevan et al, 2007)
#
#twhilton, PSU, Dec 2007

getEVI <- function(rho_nir, rho_red, rho_blue) {

  G <- 2.5
  C1 <- 6.0
  C2 <- 7.5
  L <- 1.0
  
  numer <- G * (rho_nir - rho_red)
  denom <- rho_nir + L + ( (C1*rho_red) - (C2*rho_blue) )
  EVI <- numer / denom
  return(EVI)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#getLSWI
#
#calculates land surface water index (LSWI) according to eqn 3 in Mahadevan
#   et al, 2007.  LSWI may range from -1 to 1 (Xiao et al 2004)
#
# Xiangming Xiao, Qingyuan Zhang, Bobby Braswell, Shawn Urbanski,
# Stephen Boles, Steven Wofsy, Berrien Moore III, Dennis Ojima,
# Modeling gross primary production of temperate deciduous broadleaf
# forest using satellite images and climate data, Remote Sensing of
# Environment, Volume 91, Issue 2, 30 May 2004, Pages 256-270, ISSN
# 0034-4257, 10.1016/j.rse.2004.03.010.
#
#INPUTS
#   rho_nir: near-infrared (841-876 nm) band satellite-derived reflectance
#   rho_swir:  1628-1652 nm band satellite-derived reflectance

getLSWI <- function(rho_nir, rho_swir) {
  LSWI <- (rho_nir - rho_swir) / (rho_nir + rho_swir)
  return(LSWI)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#NEE
#
#calculates VPRM net ecosystem exchange (NEE) according to eqn 12 in Mahadevan
#   et al, 2007
#INPUTS
#   rho_nir: near-infrared (841-876 nm) band satellite-derived reflectance
#   rho_red: red band satellite-derived reflectance
#   rho_blue: blue band satellite-derived reflectance
#   rho_swir:  1628-1652 nm band satellite-derived reflectance
#   T: 1xN array of observed temperatures
#   Tmin: minimum temp at which photosynthesis occurs (site-specific)
#   Tmax: maximum temp at which photosynthesis occurs
#   Topt: optimal temperature for photosynthesis
#   lambda: site-specific scaling parameter for light-dependent NEE components
#   alpha: site-specific scaling parameter for respiration
#   beta: site-specific scaling parameter for respiration
#   PAR: 1xN array of observed PAR
#   PAR_0: half-saturation value of PAR, site-specific

NEE <- function(tower, lambda=NULL, alpha=NULL, beta=NULL, PAR_0=NULL) {

  #if parameters not provided in function call, get them from the
  #   tower data frame
  if (is.null(lambda)) lambda <- tower$lambda
  if (is.null(alpha))  alpha  <- tower$alpha
  if (is.null(beta))   beta   <- tower$beta
  if (is.null(PAR_0))  PAR_0  <- tower$PAR_0

  GEE <-  (-1.0) * lambda * tower[, "Tscale"] * tower[, "Pscale"] *
    tower[, "Wscale"] * (1 / (1 + (tower[, "PAR"]/PAR_0))) *
      tower[, "EVI"] * tower[, "PAR"]
  
  R <- alpha * tower[, "Tresp"] + beta

  NEE <- GEE + R
  return(NEE)
}

NEE_sin <- function(tower, lambda=NULL, alpha=NULL, beta=NULL, PAR_0=NULL,
                    a=NULL, b=NULL, c=NULL, d=NULL) {

  #if parameters not provided in function call, get them from the
  #   tower data frame
  if (is.null(lambda)) lambda <- tower$lambda
  if (is.null(alpha))  alpha  <- tower$alpha
  if (is.null(beta))   beta   <- tower$beta
  if (is.null(PAR_0))  PAR_0  <- tower$PAR_0

  doy <- as.integer(format(as.POSIXlt(tower$date), format="%j"))
  hr <- hours(tower$date)
  
  GEE <-  (-1.0) * lambda * tower[, "Tscale"] * tower[, "Pscale"] *
    tower[, "Wscale"] * (1 / (1 + (tower[, "PAR"]/PAR_0))) *
      tower[, "EVI"] * tower[, "PAR"]
  
  R <- alpha * tower[, "Tresp"] + beta

  yr.cycle <- a * sin( (doy-b) * pi/365)
  day.cycle <- c * sin( (hr-d) * pi/24)
  
  NEE <- GEE + R + yr.cycle + day.cycle
  return(NEE)
}

NEE_SWC <- function(tower, lambda=NULL, alpha=NULL, beta=NULL, PAR_0=NULL,
                    c_swc_gee=NULL, c_swc_re=NULL) {

  #if parameters not provided in function call, get them from the
  #   tower data frame
  if (is.null(lambda)) lambda <- tower$lambda
  if (is.null(alpha))  alpha  <- tower$alpha
  if (is.null(beta))   beta   <- tower$beta
  if (is.null(PAR_0))  PAR_0  <- tower$PAR_0
  if (is.null(c_swc_gee)) c_swc_gee <- tower$c_swc_gee
  if (is.null(c_swc_re)) c_swc_re <- tower$c_swc_re

  GEE <-  (-1.0) * lambda * tower[, "Tscale"] * tower[, "Pscale"] *
    tower[, "Wscale"] * (1 / (1 + (tower[, "PAR"]/PAR_0))) *
      tower[, "EVI"] * tower[, "PAR"] * c_swc_gee * tower[, "SWC" ]
  
  R <- ( alpha * tower[, "Tresp"] + beta ) * c_swc_re * tower[, "SWC" ] 

  NEE <- GEE + R
  return(NEE)
}

#==============================================================

getGEE <- function(tow, par) {
    gee <- par$lambda * tow$Tscale * tow$Pscale * tow$Wscale * tow$EVI * 1/(1+(tow$PAR_in / par$PAR_0)) * tow$PAR_in
    return(gee)
}

GEE <- function(tower, lambda=NULL, alpha=NULL, beta=NULL, PAR_0=NULL) {

  #if parameters not provided in function call, get them from the
  #   tower data frame
  if (is.null(lambda)) lambda <- tower$lambda
  if (is.null(alpha))  alpha  <- tower$alpha
  if (is.null(beta))   beta   <- tower$beta
  if (is.null(PAR_0))  PAR_0  <- tower$PAR_0
  
  GEE <-  (-1.0) * lambda * tower[, "Tscale"] * tower[, "Pscale"] *
    tower[, "Wscale"] * (1 / (1 + (tower[, "PAR"]/PAR_0))) *
      tower[, "EVI"] * tower[, "PAR"]
  
   return(GEE)
}
