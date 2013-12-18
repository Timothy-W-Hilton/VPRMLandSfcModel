\docType{data}
\name{PFa_tower_obs}
\alias{PFa_tower_obs}
\title{Park Falls CO2 flux, PAR, and surface temperature}
\format{data frame containing ground-based observations from the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
\describe{
\item{date}{chron objects; the data timestamps}
\item{TA}{surface air temperature (C)}
\item{FC}{flux of CO2 (umol m-2 s-1)}
\item{PAR}{photosynthetically active radiation (umol m-2 s-1)}}}
\source{
  These data were extracted from the 2005 level II
  Ameriflux file for US-PFa on on 16 Dec 2013.
}
\description{
  Park Falls CO2 flux, PAR, and surface temperature
}
\note{
  PFa_tower_obs is part of the Park_Falls dataset. See
  documentation for 'Park_Falls' for further details.
}
\examples{
data(Park_Falls)
with( PFa_tower_obs, plot(date, TA,
                  ylab='T (C)', xlab='date',
                  main='Park Falls surface temperature'))
}
\seealso{
  \code{\link{Park_Falls}}
}
\keyword{datasets}

