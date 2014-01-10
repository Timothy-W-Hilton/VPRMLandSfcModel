\name{optimizeVPRM_DE}
\alias{optimizeVPRM_DE}
\title{run DEoptim to estimate VPRM parameter values}
\usage{
  optimizeVPRM_DE(driver_data, DEcontrol, refresh = 5,
    msg = "", lambda_prior = c(0, 1.5),
    alpha_prior = c(0, 1.5), beta_prior = c(-4, 4),
    PAR0_prior = c(0.1, 6000))
}
\arguments{
  \item{driver_data}{May be a VPRM_driver_data object or a
  data frame.  If a data frame, driver_data must contain
  the variables Tscale, Pscale, Wscale, EVI, and PAR, and
  NEE_obs.}

  \item{DEcontrol}{DEoptim.control object}

  \item{refresh}{how many DE iterations in between progress
  writeouts}

  \item{msg}{character string; message to be displayed
  before DE is called.  Useful for log files.}

  \item{lambda_prior}{2-element vector; upper and lower
  lambda values for optimization.  Default is c(0.0, 1.5).}

  \item{alpha_prior}{2-element vector; upper and lower
  alpha values for optimization.  Default is c(0.0, 1.5).}

  \item{beta_prior}{2-element vector; upper and lower beta
  values for optimization.  Default is c(-4.0, 4.0).}

  \item{PAR0_prior}{2-element vector; upper and lower PAR_0
  values for optimization.  Default is c(0.1, 6000).}
}
\value{
  DEoptim object. DEoptim output for best VPRM parameter
  estimate.
}
\description{
  define VPRM parameter windows (upper and lower) and call
  DEoptim to estimate VPRM parmeters.  Helper function for
  estimate_VPRM_pars.
}
\examples{
data(Park_Falls)
}
\author{
  Timothy W. Hilton
}

