\name{getAbsWeightedErr}
\alias{getAbsWeightedErr}
\title{calculate sum of absolute errors.}
\usage{
  getAbsWeightedErr(x)
}
\arguments{
  \item{x}{error values}
}
\value{
  sum of absolute errors in x.
}
\description{
  calculate sum of absolute errors, ignoring NAs.
}
\details{
  if there are no valid (non-NA) data points, return a very
  high value (currently 1e20).  This allows use with
  DEoptim, which issues an error if a cost function returns
  NA.
}
\author{
  Timothy W. Hilton
}

