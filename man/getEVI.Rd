\name{getEVI}
\alias{getEVI}
\title{calculate EVI}
\usage{
  getEVI(rho_nir, rho_red, rho_blue)
}
\arguments{
  \item{rho_nir}{1xN numeric vector; near-infrared (841-876
  nm) band satellite-derived reflectance}

  \item{rho_red}{1xN numeric vector; red band
  satellite-derived reflectance}

  \item{rho_blue}{1xN numeric vector; blue band
  satellite-derived reflectance}
}
\value{
  numeric; EVI term in VPRM equation (eqn 12 in Mahadevan
  et al, 2007)
}
\description{
  calculates EVI from MODIS reflectances according to eqn 2
  in Mahadevan et al, 2007
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

