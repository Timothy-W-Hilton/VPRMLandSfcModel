\name{detect_large_greenness_change_periods}
\alias{detect_large_greenness_change_periods}
\title{implememnts an approximation of the methods of Zhang et al
(2003, 2006).}
\usage{
  detect_large_greenness_change_periods(data, verbose = 0)
}
\arguments{
  \item{data}{data frame with (at least) columns "sitecode"
  (unique sitecode), "t" (chron object timestamps) and
  "EVI" (EVI observations). Data should contain EVI
  observations from one and only one site.}

  \item{verbose}{integer; causes silent execution if 0
  (default); larger values cause increasingly verbose
  printouts.  Currently has two options: silent if 0;
  prints site being processed if >0; this may change in the
  future to provide more granularity.}
}
\value{
  data frame with columns "idx" (row indices of data where
  slope of EVI change changes sign), "date" (chron object
  containing the timestamps corresponding to idx), and
  "phen" (factor encoding the type of EVI change, with
  levels ginc, gdec, gmin, gmax.
}
\description{
  locate periods of sustained greeness increase or decrease
  within an EVI time series
}
\examples{
data(Park_Falls)
names(PFa_evi) <- c('t', 'EVI', 'sitecode')
PFa_evi[['sitecode']] <- "US-PFa"
print(head(PFa_evi))
phen_transition_dates <- detect_large_greenness_change_periods(PFa_evi)
print(head(phen_transition_dates))
}
\author{
  Timothy W. Hilton
}
\references{
  Xiaoyang Zhang, Mark A. Friedl, Crystal B. Schaaf, Alan
  H. Strahler, John C.F. Hodges, Feng Gao, Bradley C. Reed,
  Alfredo Huete, Monitoring vegetation phenology using
  MODIS, Remote Sensing of Environment, Volume 84, Issue 3,
  March 2003, Pages 471-475, ISSN 0034-4257,
  http://dx.doi.org/10.1016/S0034-4257(02)00135-9.

  Zhang, X., M. A. Friedl, and C. B. Schaaf (2006), Global
  vegetation phenology from Moderate Resolution Imaging
  Spectroradiometer (MODIS): Evaluation of global patterns
  and comparison with in situ measurements, J. Geophys.
  Res., 111, G04017, doi:10.1029/2006JG000217.
}

