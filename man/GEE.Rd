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

