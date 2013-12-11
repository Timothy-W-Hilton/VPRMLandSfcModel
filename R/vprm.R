##' calculates Tscale according to eqn 6 in Mahadevan et al, 2007
##'
##' @title calculate Tscale
##' @param T T: 1xN array of observed temperatures (C)
##' @param Tmax maximum temp at which photosynthesis occurs (C)
##' @param Tmin minimum temp at which photosynthesis occurs (C)
##' @param Topt optimal temperature for photosynthesis (C)
##' @return Tscale term in VPRM equation (eqn 12 in Mahadevan et al,
##' 2007)
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @author Timothy W. Hilton
##' @export 
getTscale <- function (T, Tmax, Tmin, Topt) 
{
  numer <- (T - Tmin) * (T - Tmax)
  denom <- ((T - Tmin) * (T - Tmax)) - (T - Topt)^2
  Tscale = numer/denom
  Tscale[which(T < Tmin)] <- 0
  Tscale[which(T > Tmax)] <- 0
  Tscale[ Tscale < 0 ] <- 0
  Tscale[ Tscale > 1 ] <- 1
  return(Tscale)
}

##' calculates Pscale according to eqn 7 in Mahadevan et al, 2007
##'
##' @title calculate Pscale
##' @param LSWI land surface water index
##' @param phen MODIS phenology (factor with levels ginc, gmin, gmax, gdec)
##' @return Pscale term in VPRM equation (eqn 12 in Mahadevan et al, 2008)
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @export 
getPscale <- function( LSWI, phen ) {
  
  Pscale <- (1 + LSWI)/2
  Pscale[ phen == 'gmax' ] <- 1.0
  return( Pscale )

}

##' calculates Wscale according to eqn 8 in Mahadevan et al, 2007
##'
##' @title calculate Wscale
##' @param LSWI land surface water index
##' @param LSWI_max maximum LSWI for site
##' @return Pscale term in VPRM equation (eqn 12 in Mahadevan et al, 2007)
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @export 
getWscale <- function(LSWI, LSWI_max) {
  Wscale <- (1 + LSWI) / (1 + LSWI_max)
  return(Wscale)
}

##' calculates EVI from MODIS reflectances according to eqn 2 in
##' Mahadevan et al, 2007
##' 
##' @title calculate EVI
##' @param rho_nir numeric vector; near-infrared (841-876 nm) band
##' satellite-derived reflectance
##' @param rho_red numeric vector; red band satellite-derived
##' reflectance
##' @param rho_blue numeric vector; blue band satellite-derived
##' reflectance
##' @return numeric; EVI term in VPRM equation (eqn 12 in Mahadevan et al, 2007)
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @export 
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

##' calculates land surface water index (LSWI) according to eqn 3 in
##' Mahadevan et al, 2007.
##' 
##' LSWI may range from -1 to 1 (Xiao et al 2004).
##' @title calculate LSWI
##' @param rho_nir numeric vector; near-infrared (841-876 nm) band satellite-derived reflectance
##' @param rho_swir  numeric vector; 1628-1652 nm band satellite-derived reflectance
##' @return LSWI: numeric vector same size as rho_nir and rho_swir
##' containing LSWI values.
##' @author Timothy W. Hilton
##' @references Xiangming Xiao, Qingyuan Zhang, Bobby Braswell, Shawn
##' Urbanski, Stephen Boles, Steven Wofsy, Berrien Moore III, Dennis
##' Ojima, Modeling gross primary production of temperate deciduous
##' broadleaf forest using satellite images and climate data, Remote
##' Sensing of Environment, Volume 91, Issue 2, 30 May 2004, Pages
##' 256-270, ISSN 0034-4257, 10.1016/j.rse.2004.03.010.
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @export 
getLSWI <- function(rho_nir, rho_swir) {
  LSWI <- (rho_nir - rho_swir) / (rho_nir + rho_swir)
  return(LSWI)
}


##' calculates VPRM net ecosystem exchange (NEE) according to eqn 12
##' in Mahadevan et al, 2007
##'
##' Arguments lambda, alpha, beta, and PAR_0 may be omitted from the
##' function call.  In this case they must be present as variables in
##' data frame tower.
##'
##' The Tresp variable in tower is the temperature used to calculate
##' respiration.  Tresp should be max(Tair, Tlow), where Tair is the
##' air temperature (deg C) and Tlow is the minimum air temperature
##' (deg C) for respiration.  This is explained more fully in
##' Mahadevan et al (2008) section 2.2.
##' @title calculate VPRM NEE
##' @param tower data frame containing (at least) variables Tscale,
##' Pscale, Wscale, EVI, PAR, Tresp.
##' @param lambda numeric; VPRM parameter: maximum light use efficiency 
##' @param alpha numeric; VPRM parameter (slope of respiration with
##' respect to temperature)
##' @param beta numeric; VPRM parameter (basal respiration rate)
##' @param PAR_0 numeric; VPRM parameter (LUE half-saturation value)
##' @return vector of same length as number of rows in tower containin
##' VPRM NEE \[umol m-2 s-1\]
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @export 
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

##' calculate VPRM GEE according to Mahadevan et al (2008) eq. 4
##'
##' @title calculate VPRM GEE
##' @inheritParams NEE
##' @return vector of same length as number of rows in tower containin
##' VPRM GEE \[umol m-2 s-1\]
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respira- tion Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008. 
##' @export 
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
