\name{getWscale}
\alias{getWscale}
\title{calculate Wscale}
\usage{
  getWscale(LSWI, LSWI_max)
}
\arguments{
  \item{LSWI}{numeric vector; land surface water index}

  \item{LSWI_max}{numeric; maximum LSWI for site (that is,
  a single value)}
}
\value{
  Pscale term in VPRM equation (eqn 12 in Mahadevan et al,
  2007)
}
\description{
  calculates Wscale according to eqn 8 in Mahadevan et al,
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
  Vegetation Photosynthesis and Respiration Model (VPRM),
  Global Biogeochem. Cy., 22, GB2005,
  doi:10.1029/2006GB002735, 2008.
}

