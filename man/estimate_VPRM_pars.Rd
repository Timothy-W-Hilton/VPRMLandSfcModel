\name{estimate_VPRM_pars}
\alias{estimate_VPRM_pars}
\title{estimate VPRM parameter values by minimizing SSE}
\usage{
  estimate_VPRM_pars(all_data, opt_groups = NULL,
    DE_itermax, out_path, par_set_str = "",
    run_parallel = FALSE, lambda_prior = c(0, 1.5),
    alpha_prior = c(0, 1.5), beta_prior = c(-4, 4),
    PAR0_prior = c(0.1, 6000))
}
\arguments{
  \item{all_data}{data frame; VPRM input data}

  \item{opt_groups}{list of factors -- data will be split
  by the levels of opt_groups and each combination
  optimized separately. Groups containing less than six
  observations are ignored.}

  \item{DE_itermax}{integer, maximum differential evolution
  iterations}

  \item{out_path}{character string; path to directory in
  which to save the estimated parameters}

  \item{par_set_str}{character string; phrase to denote the
  parameterization being considered}

  \item{run_parallel}{TRUE|{FALSE}; if true, use Rmpi to
  run optimization jobs in parallel}

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
  0 on success.
}
\description{
  This is the main function for VPRM parameter estimation.
  Attaches data frame containing VPRM input data for a
  single tower site
}
\author{
  Timothy W. Hilton
}

