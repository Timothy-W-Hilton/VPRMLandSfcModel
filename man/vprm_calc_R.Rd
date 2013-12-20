\name{vprm_calc_R}
\alias{vprm_calc_R}
\title{calculate VPRM ecosystem respiration}
\usage{
  vprm_calc_R(driver_data, alpha = NULL, beta = NULL)
}
\arguments{
  \item{driver_data}{May be a VPRM_driver_data object or a
  data frame.  If a data frame, driver_data must contain
  the variables Tscale, Pscale, Wscale, EVI, PAR, and
  Tresp.  The variables alpha and beta are optional (see
  'details').}

  \item{alpha}{numeric, optional; VPRM parameter (slope of
  respiration with respect to temperature)}

  \item{beta}{numeric, optional; VPRM parameter (basal
  respiration rate)}
}
\value{
  vector of same length as number of rows in driver_data
  containin VPRM ecosystem respiration [umol m-2 s-1]
}
\description{
  calculates VPRM ecosystem respiration (R) according to
  eqn 10 in Mahadevan et al, 2007
}
\details{
  Arguments alpha and beta may be omitted from the function
  call. In this case they must be present as variables in
  data frame driver_data.  If either of these parameters
  are present in driver_data *and* specified as parameters
  in the function call the function parameter values will
  be used and the values in driver_data will be ignored.
  If specifed as function parameters alpha and beta may be
  single values or numeric vectors the same length as the
  number of observations in driver_data.

  The Tresp variable in driver_data is the temperature used
  to calculate respiration.  Tresp should be max(Tair,
  Tlow), where Tair is the air temperature (deg C) and Tlow
  is the minimum air temperature (deg C) for respiration.
  This is explained more fully in Mahadevan et al (2008)
  section 2.2.
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

data(VPRM_parameters)
attach(all_all_VPRM_parameters)
ER <- vprm_calc_R(pfa_dd, alpha=alpha, beta=beta)
}
\author{
  Timothy W. Hilton
}
\references{
  Hilton, T. W., Davis, K. J., Keller, K., and Urban, N.
  M.: Improving North American terrestrial CO2 flux
  diagnosis using spatial structure in land surface model
  residuals, Biogeosciences, 10, 4607-4625,
  doi:10.5194/bg-10-4607-2013, 2013.

  Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
  A., Lin, J., Gerbig, C., Munger, J., Chow, V., and
  Gottlieb, E.: A satellite-based biosphere
  parameterization for net ecosystem CO2 exchange:
  Vegetation Photosynthesis and Respiration Model (VPRM),
  Global Biogeochem. Cy., 22, GB2005,
  doi:10.1029/2006GB002735, 2008.
}

