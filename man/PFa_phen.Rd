\docType{data}
\name{PFa_phen}
\alias{PFa_phen}
\title{Park Falls phenology transition data}
\format{data frame containing MODIS enhanced vegetation index (EVI) for the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
\describe{
\item{date}{chron object; timestamps (date and time)}
\item{phen}{factor; phenology dynamics.  levels are ginc (onset greenness increase), gdec (onset greenness decrease), gmin (onset greenness minimum), gmax (onset greenness maximum).}
}}
\source{
  These data are extracted from the Oak Ridge National
  Laboratory MODIS subsets for MODIS product MOD12Q2.
}
\description{
  Park Falls phenology transition data
}
\note{
  PFa_phen is part of the Park_Falls dataset. See
  documentation for 'Park_Falls' for further details.
}
\examples{
data(Park_Falls)
print(PFa_phen)
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

