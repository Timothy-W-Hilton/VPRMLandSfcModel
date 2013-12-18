\docType{data}
\name{PFa_refl}
\alias{PFa_refl}
\title{Park Falls MODIS reflectance data}
\format{data frame containing MODIS reflectances for the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
\describe{
\item{date}{chron object; timestamps (date and time)}
\item{red}{the reflectance value for the red band}
\item{nir}{the reflectance value for the near infrared (NIR) band}
\item{blue}{the reflectance value for the blue band}
\item{swir}{the reflectance value for the shortwave infrade (SWIR) band}
}}
\source{
  These data are extracted from the Oak Ridge National
  Laboratory MODIS subsets for MODIS product MOD09A1.
}
\description{
  Park Falls MODIS reflectance data
}
\note{
  PFa_refl is part of the Park_Falls dataset. See
  documentation for 'Park_Falls' for further details.
}
\examples{
data(Park_Falls)
with(PFa_refl, plot(date, red,
                    col='red', type='b', pch='o',
                    xlab='date', ylab='reflectance',
                    ylim=range(PFa_refl[ , c('red', 'blue')],
                               na.rm=TRUE),
                    main='Park Falls MODIS reflectance'))
with(PFa_refl, points(date, blue,
                      col='blue', type='b', lty='dashed', pch='*'))
legend( x='topright',
        legend=c('red band', 'blue band'),
        col=c('red', 'blue'), lty=c('solid', 'dashed'), pch=c('o', '*'))
}
\references{
  Oak Ridge National Laboratory Distributed Active Archive
  Center (ORNL DAAC). 2011. MODIS subsetted land products,
  Collection 5. Available on-line
  [http://daac.ornl.gov/MODIS/modis.html] from ORNL DAAC,
  Oak Ridge, Tennessee, U.S.A. Accessed 17 Dec, 2013.
}
\seealso{
  \code{\link{Park_Falls}}
}
\keyword{datasets}

