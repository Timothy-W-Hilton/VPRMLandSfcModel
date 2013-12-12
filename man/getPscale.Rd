\name{getPscale}
\alias{getPscale}
\title{calculate Pscale}
\usage{
  getPscale(LSWI, phen)
}
\arguments{
  \item{LSWI}{1xN numeric vector; land surface water index}

  \item{phen}{1xN factor vector; MODIS phenology (factor
  with levels ginc, gmin, gmax, gdec)}
}
\value{
  Pscale term in VPRM equation (eqn 12 in Mahadevan et al,
  2008)
}
\description{
  calculates Pscale according to eqn 7 in Mahadevan et al,
  2007
}
\details{
  The levels of phen are abbreviations: ginc - "onset
  greenness increase", gmin - "onset greenness minimum",
  gmax - "onset greenness maximum", gdec - "onset greenness
  decrease".  This phenology dynamics classification scheme
  is explained more fully in Zhang et al (2003).
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

  Xiaoyang Zhang, Mark A. Friedl, Crystal B. Schaaf, Alan
  H. Strahler, John C.F. Hodges, Feng Gao, Bradley C. Reed,
  Alfredo Huete, Monitoring vegetation phenology using
  MODIS, Remote Sensing of Environment, Volume 84, Issue 3,
  March 2003, Pages 471-475, ISSN 0034-4257,
  http://dx.doi.org/10.1016/S0034-4257(02)00135-9.
}

