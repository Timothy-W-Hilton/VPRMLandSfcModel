\name{interpMODIS}
\alias{interpMODIS}
\title{interpolate MODIS band data to provide values at a given set of dates}
\usage{
  interpMODIS(mod_date, mod_val, out_date, method)
}
\arguments{
  \item{mod_date}{chron vector; MODIS Time stamp}

  \item{mod_val}{MODIS values}

  \item{out_date}{chron vector; the dates for the output}

  \item{method}{string, either "linear" or "constant"}
}
\value{
  list with components "date" and "val", containing
  timestamps (class chron) and values (class is dependent
  on the MODIS product) of the interpolated data.
}
\description{
  interpolate MODIS band data to provide values at a given
  set of dates using either linear or constant
  interpolation.
}
\author{
  Timothy W. Hilton
}

