\name{NEE_sin}
\alias{NEE_sin}
\title{VPRM NEE with added daily and annual cycles}
\usage{
  NEE_sin(tower, lambda = NULL, alpha = NULL, beta = NULL,
    PAR_0 = NULL, a = NULL, b = NULL, c = NULL, d = NULL)
}
\arguments{
  \item{a}{amplitude for annual cycle sinusoid [umol m-2
  s-1]}

  \item{b}{offset for annual cycle sinusoid, [days]}

  \item{c}{amplitude for daily cycle sinusoid [umol m-2
  s-1]}

  \item{d}{offset for daily cycle sinusoid, [hours]}

  \item{tower}{data frame containing (at least) variables
  Tscale, Pscale, Wscale, EVI, PAR, Tresp.}

  \item{lambda}{numeric; VPRM parameter: maximum light use
  efficiency}

  \item{alpha}{numeric; VPRM parameter (slope of
  respiration with respect to temperature)}

  \item{beta}{numeric; VPRM parameter (basal respiration
  rate)}

  \item{PAR_0}{numeric; VPRM parameter (LUE half-saturation
  value)}
}
\value{
  vector of same length as number of rows in tower
  containin VPRM NEE \[umol m-2 s-1\]
}
\description{
  Similar to NEE, but adds a sinusoidal daily and annual
  cycle to VPRM structure.  That is, NEE_sin calculates
  VPRM NEE + VPRM R + yr_cycle + daily_cycle.  This is a
  simple method to get VPRM to focus on NEE anomalies
  rather than NEE.
}
\details{
  The annual cycle is in the form a * sin( DOY-b ), where
  DOY is day of year and a and b are parameters to adjust
  the amplitude and seasonal timing, respectively.  The
  daily cycle is in the form c * sin( hr-d ), where hr is
  hour of the day and c and d are parameters to adjust the
  amplitude and daily timing, respectively.
}
\author{
  Timothy W. Hilton
}
\references{
  Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
  A., Lin, J., Gerbig, C., Munger, J., Chow, V., and
  Gottlieb, E.: A satellite-based biosphere
  parameterization for net ecosystem CO2 exchange:
  Vegetation Photosynthesis and Respira- tion Model (VPRM),
  Global Biogeochem. Cy., 22, GB2005,
  doi:10.1029/2006GB002735, 2008.
}

