\name{NEE}
\alias{NEE}
\title{calculate VPRM NEE}
\usage{
  NEE(driver_data, lambda = NULL, alpha = NULL,
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
  containin VPRM NEE \[umol m-2 s-1\]
}
\description{
  calculates VPRM net ecosystem exchange (NEE) according to
  eqn 12 in Mahadevan et al, 2007
}
\details{
  Arguments lambda, alpha, beta, and PAR_0 may be omitted
  from the function call.  In this case they must be
  present as variables in data frame driver_data.  If any
  of these four parameters are present in driver_data *and*
  specified as parameters in the function call the function
  parameter values will be used and the values in
  driver_data will be ignored.  If specifed as function
  parameters lambda, alpha, beta, and PAR_0 may be single
  values or numeric vectors the same length as the number
  of observations in driver_data.

  The Tresp variable in driver_data is the temperature used
  to calculate respiration.  Tresp should be max(Tair,
  Tlow), where Tair is the air temperature (deg C) and Tlow
  is the minimum air temperature (deg C) for respiration.
  This is explained more fully in Mahadevan et al (2008)
  section 2.2.
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

