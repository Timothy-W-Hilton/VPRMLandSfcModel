\name{getPscale}
\alias{getPscale}
\title{calculate Pscale}
\usage{
  getPscale(LSWI, phen)
}
\arguments{
  \item{LSWI}{land surface water index}

  \item{phen}{MODIS phenology (factor with levels ginc,
  gmin, gmax, gdec)}
}
\value{
  Pscale term in VPRM equation (eqn 12 in Mahadevan et al,
  2008)
}
\description{
  calculates Pscale according to eqn 7 in Mahadevan et al,
  2007
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

