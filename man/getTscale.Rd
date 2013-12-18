\name{getTscale}
\alias{getTscale}
\title{calculate Tscale}
\usage{
  getTscale(T, Tmax, Tmin, Topt)
}
\arguments{
  \item{T}{T: 1xN array of observed temperatures (C)}

  \item{Tmax}{maximum temp at which photosynthesis occurs
  (C)}

  \item{Tmin}{minimum temp at which photosynthesis occurs
  (C)}

  \item{Topt}{optimal temperature for photosynthesis (C)}
}
\value{
  Tscale term in VPRM equation (eqn 12 in Mahadevan et al,
  2007)
}
\description{
  calculates Tscale according to eqn 6 in Mahadevan et al,
  2007
}
\examples{
data(Park_Falls)
Tscale <- getTscale( PFa_tower_obs[['TA']], Tmax=40, Tmin=0, Topt=20 )
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

