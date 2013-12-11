\name{NEE_SWC}
\alias{NEE_SWC}
\title{calculate VPRM NEE with soil water content added to VPRM structure}
\usage{
  NEE_SWC(tower, lambda = NULL, alpha = NULL, beta = NULL,
    PAR_0 = NULL, c_swc_gee = NULL, c_swc_re = NULL)
}
\arguments{
  \item{c_swc_gee}{VPRM_SWC parameter; soil water content
  multiplier for GEE}

  \item{c_swc_re}{VPRM_SWC parameter; soil water content
  multiplier for respiration}

  \item{tower}{data frame containing (at least) variables
  Tscale, Pscale, Wscale, EVI, PAR, Tresp.}

  \item{lambda}{numeric; VPRM parameter: maximum light use
  efficiency}

  \item{alpha}{numeric; VPRM parameter (slope of
  respiration with respect to temperature)}

  \item{beta}{numeric; VPRM parameter (basal respiration
  rate)}

  \item{PAR_0}{numeric; VPRM parameter (LUE half-saturation
  value)}
}
\value{
  vector of same length as number of rows in tower
  containining NEE
}
\description{
  adjusts VPRM model structure to include soil water
  content (SWC) in NEE calculation.
}
\author{
  Timothy W. Hilton
}

