\docType{data}
\name{Park_Falls}
\alias{Park_Falls}
\title{Park Falls, Wisconsin, USA eddy covariance data and MODIS data}
\format{The dataset Park_Falls contains four data frames:
\describe{
\item{PFa_evi}{timestamped MODIS EVI from NASA's Terra satellite (MODIS product MOD13Q1).}
\item{PFa_refl}{timestamped MODIS surface reflectances in the red, blue, near infrared (NIR), and shortwave infrared (SWIR) bands (MODIS product MCD43A4).}
\item{PFa_phen}{timestamped MODIS phenology dynamics data (MODIS product MOD12Q2)}
\item{PFa_tower_obs}{timestamped air temperature (TA), net ecosystem exchange of carbon dioxide (FC), and photosynthetically active radiation (PAR) data.}
}}
\source{
  The data in PFa_tower_obs are from the Park Falls,
  Wisconsin Ameriflux level II dataset
  (\url{ameriflux.lbl.gov}).  MODIS data are from the
  Ameriflux site MODIS snapshots provided by Oak Ridge
  National Laboratory (\url{http://daac.ornl.gov/MODIS/}).
}
\description{
  The dataset Park_Falls contains MODIS observations as
  well as ground-based eddy covariance flux tower data for
  the Ameriflux tall tower located in Park Falls,
  Wisconsin, USA.  The data span the period 1 May 2005
  through 31 July 2005.
}
\examples{
data(Park_Falls)
with( PFa_tower_obs, plot(date, TA,
                  ylab='T (C)', xlab='date',
                  main='Park Falls surface temperature'))
with( PFa_evi, plot(date, evi,
                    ylab='MODIS EVI', xlab='date',
                    main='Park Falls EVI' ))
}
\author{
  Timothy W. Hilton
}
\references{
  MODIS snapshots: \url{http://daac.ornl.gov/MODIS/}

  The Park Falls, Wisconsin, USA Ameriflux site:
  \url{ameriflux.lbl.gov}, Berger et al (2000).

  Berger, Bradford W., Kenneth J. Davis, Chuixiang Yi,
  Peter S. Bakwin, Cong Long Zhao, 2001: Long-Term Carbon
  Dioxide Fluxes from a Very Tall Tower in a Northern
  Forest: Flux Measurement Methodology. J. Atmos. Oceanic
  Technol., 18, 529-542.
}
\seealso{
  \code{\link{PFa_tower_obs}}, \code{\link{PFa_evi}},
  \code{\link{PFa_refl}}, \code{\link{PFa_phen}}
}
\keyword{datasets}

