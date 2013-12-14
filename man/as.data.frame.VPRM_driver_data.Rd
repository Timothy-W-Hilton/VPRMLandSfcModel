\name{as.data.frame.VPRM_driver_data}
\alias{as.data.frame.VPRM_driver_data}
\title{coerce to a data frame}
\usage{
  \method{as.data.frame}{VPRM_driver_data} (x, ...)
}
\arguments{
  \item{x}{VPRM_driver_data object}

  \item{...}{additional arguments to be passed to or from
  methods.}
}
\value{
  data frame with fields variables NEE_obs, T, PAR,
  rho_nir, rho_swir, EVI, and phen.
}
\description{
  method for VPRM_driver_data
}
\details{
  The data field of the VPRM_driver_data object is kept.
  Other fields (short name, long name, etc.) are discarded.
}
\author{
  Timothy W. Hilton
}

