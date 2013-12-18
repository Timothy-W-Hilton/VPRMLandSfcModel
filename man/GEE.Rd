\name{GEE}
\alias{GEE}
\title{calculate VPRM GEE}
\usage{
  GEE(driver_data, lambda = NULL, alpha = NULL,
    beta = NULL, PAR_0 = NULL)
}
\arguments{
  \item{driver_data}{May be a VPRM_driver_data object or a
  data frame.  If a data frame, driver_data must contain
  the variables Tscale, Pscale, Wscale, EVI, PAR, and
  Tresp.  The variables lambda, alpha, beta, and PAR_0 are
  optional (see 'details').}

  \item{lambda}{numeric, optional; VPRM parameter: maximum
  light use efficiency.}

  \item{alpha}{numeric, optional; VPRM parameter (slope of
  respiration with respect to temperature)}

  \item{beta}{numeric, optional; VPRM parameter (basal
  respiration rate)}

  \item{PAR_0}{numeric, optional; VPRM parameter (LUE
  half-saturation value)}
}
\value{
  vector of same length as number of rows in driver_data
  containin VPRM GEE \[umol m-2 s-1\]
}
\description{
  calculate VPRM GEE according to Mahadevan et al (2008)
  eq. 4
}
\examples{
data(Park_Falls)
pfa_dd <- VPRM_driver_data(name_long="Park Falls",
                           name_short = "US-PFa",
                           lat=45.9459,
                           lon=-90.2723,
                           PFT='MF',
                           tower_date=PFa_tower_obs[['date']],
                           NEE_obs=PFa_tower_obs[['FC']],
                           T=PFa_tower_obs[['TA']],
                           PAR=PFa_tower_obs[['PAR']],
                           date_nir = PFa_refl[['date']],
                           rho_nir=PFa_refl[['nir']],
                           date_swir = PFa_refl[['date']],
                           rho_swir = PFa_refl[['swir']],
                           date_EVI = PFa_evi[['date']],
                           EVI=PFa_evi[['evi']],
                           phen=NA)
#all-sites--all-time VPRM parameters from Hilton et al (2013)
lambda <- 0.06940179
PAR_0 <- 1104.81
alpha <- 0.04672012
beta <- 0.5260401
GEE <- GEE(pfa_dd, lambda=lambda, PAR_0=PAR_0, alpha=alpha, beta=beta)
}
\author{
  Timothy W. Hilton
}
\references{
  Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
  A., Lin, J., Gerbig, C., Munger, J., Chow, V., and
  Gottlieb, E.: A satellite-based biosphere
  parameterization for net ecosystem CO2 exchange:
  Vegetation Photosynthesis and Respiration Model (VPRM),
  Global Biogeochem. Cy., 22, GB2005,
  doi:10.1029/2006GB002735, 2008.
}

