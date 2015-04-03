##' This is the main function for VPRM parameter estimation.  It
##' optimizes VPRM parameters to a set of set of provided net
##' ecosystem exchange (NEE) data by minimizing sum of squared errors
##' (SSE) between VPRM and the provided NEE.  A joint optimization of
##' all four VPRM parameters is performed using the differential
##' evolution global optimization algorithm as implemented by the
##' DEoptim package.
##'
##' @title estimate VPRM parameter values by minimizing SSE
##' @param all_data data frame; VPRM driver data to drive the
##' parameter estimation.  Should usually be the 'data' field of a
##' VPRM_driver_data object.
##' @param opt_groups list of factors -- data will be split by the
##' levels of opt_groups and each combination optimized separately.
##' Groups containing less than six observations are ignored.  If
##' unspecified parameters are estimated for all of all_data.
##' @param DE_itermax integer, maximum differential evolution
##' iterations (see DEoptim documentation).  Should be something like
##' 800 for a production run -- this will take some time for a typical
##' data set.  Run with a small number (5 or so) for a debugging run.
##' @param out_path character string; path to directory in which to
##' save the estimated parameters.  Default is the value of getwd()
##' @param par_set_str character string; phrase to denote the
##' parameterization being considered
##' @param run_parallel TRUE|{FALSE}; if true, use Rmpi to run
##' optimization jobs in parallel
##' @param lambda_prior 2-element vector; upper and lower lambda
##' values for optimization.  Default is c(0.0, 1.5).
##' @param alpha_prior  2-element vector; upper and lower alpha
##' values for optimization.  Default is c(0.0, 1.5).
##' @param beta_prior  2-element vector; upper and lower beta
##' values for optimization.  Default is c(-4.0, 4.0).
##' @param PAR0_prior  2-element vector; upper and lower PAR_0
##' values for optimization.  Default is c(0.1, 6000).
##' @return 0 on success.  A list of DEoptim objects (see DEoptim
##' documentation), one for each unique combination of factors in
##' opt_groups, is written to an RData file in the directory specified
##' by out_path.  The file is named in the format
##' ParEst_PAR_SET_STR.de.RData, where PAR_SET_STR is the value of the
##' par_set_str argument to estimate_VPRM_pars.  The best fit
##' parameter values are in a named vector in the
##' [['optim']][['bestmem']] field of the DEoptim objects.  See
##' DEoptim documentation for further interpretation of DEoptim
##' objects.
##' @author Timothy W. Hilton
##' @import DEoptim
##' @export
##' @examples
##' data(Park_Falls)
##' pfa_dd <- VPRM_driver_data(name_long="Park Falls",
##'                            name_short = "US-PFa",
##'                            lat=45.9459,
##'                            lon=-90.2723,
##'                            PFT='MF',
##'                            tower_date=PFa_tower_obs[['date']],
##'                            NEE_obs=PFa_tower_obs[['FC']],
##'                            T=PFa_tower_obs[['TA']],
##'                            PAR=PFa_tower_obs[['PAR']],
##'                            date_nir = PFa_refl[['date']],
##'                            rho_nir=PFa_refl[['nir']],
##'                            date_swir = PFa_refl[['date']],
##'                            rho_swir = PFa_refl[['swir']],
##'                            date_EVI = PFa_evi[['date']],
##'                            EVI=PFa_evi[['evi']],
##'                            phen=NA)
##' par_est_status <- estimate_VPRM_pars(all_data=pfa_dd[['data']],
##'                                      DE_itermax = 2,
##'                                      par_set_str='ExampleRun')
##' par_est_status <-
##'     estimate_VPRM_pars(all_data=pfa_dd[['data']],
##'                        DE_itermax = 2,
##'                        par_set_str='ExampleRun_Monthly',
##'                        opt_groups=months(pfa_dd[['data']][['date']]))
estimate_VPRM_pars <- function(all_data,
                               opt_groups=NULL,
                               DE_itermax,
                               out_path=getwd(),
                               par_set_str="",
                               run_parallel=FALSE,
                               lambda_prior=c(0.0, 1.5),
                               alpha_prior=c(0.0, 1.5),
                               beta_prior=c(-4.0, 4.0),
                               PAR0_prior=c(0.1 , 6000)) {

  if ( !is.null( opt_groups ) ) {
    chunk_list <- split( all_data, f=opt_groups )
    chunk_list <- list.trim(chunk_list, bool.FUN=function(x) nrow(x) > 5)
  } else {
    chunk_list <- list( all=all_data )
  }

  cat("list elements for DE: ", length(chunk_list), "\n")
  cat("list names: ", names(chunk_list), "\n")

  ctl <- DEoptim.control(itermax=DE_itermax, trace=TRUE)

  ## -----
  ## wrapper allows optimizeVPRM_DE to print a message with the chunk name
  opt_FUN <- function( .name ) {
    optimizeVPRM_DE( chunk_list[[ .name ]],
                    DEcontrol=ctl,
                    msg=paste( '-------\nbeginning ', .name, '\n'),
                    lambda_prior=lambda_prior,
                    alpha_prior=alpha_prior,
                    beta_prior=beta_prior,
                    PAR0_prior=PAR0_prior) }
  ## -----

  ## if parallel run is requested, check to see if Rmpi is installed.
  ## If it is not, issue a (caught) error and run sequentially.
  if ( run_parallel ) {
      result <- tryCatch(
          library('Rmpi'),
          warning = function(w) {
              cat("library('Rmpi') produced a warning")
          },
          error = function(e) {
              cat(paste("Parallel execuation was requested, but Rmpi",
                        "package could not be loaded.  Perhaps Rmpi",
                        "is not installed?",
                        "Falling back to sequential execution."))
              run_parallel <- FALSE
          },
          finally = {
              cat("finally block")
          })
  }

  if( run_parallel ) {
      ## parallel was requested *and* Rmpi is installed, so
      ## proceed with parallel run.
      pars <- mpi.parLapply( names( chunk_list ), opt_FUN )
  } else {
      ## run sequentially -- either parallel was not requested or Rmpi
      ## is not installed
      pars <- lapply( names( chunk_list ), opt_FUN )
  }

  ## because ***apply was called on the names( chunk list ), the names
  ## of pars are now  names( names( chunklist ) ), which is empty.
  names( pars ) <- names( chunk_list )

  ## save the estimated parameters
  if (!file.exists(out_path))
    dir.create(out_path, recursive=TRUE)
  outfile <- file.path(out_path, paste("ParEst_", par_set_str, ".de.RData", sep=""))
  discard <- with(pars, save(list=names(pars), file=outfile))
  cat(paste("wrote", outfile, "\n"))
  return(0)
}

##============================================================
##---------"low level" parameter estimation functions --------
##============================================================

##==============================================================
##
##' computes likelihood function for residuals in x uses eq 4.54 from
## Wilks, 1995 (eqn 4.22 in Wilks, 2006)
##'
##' @title compute residual likelihood function
##' @param x numeric, residual values
##' @param mu (check on this -- TWH)
##' @param sigma (check on this -- TWH)
##' @return likelihood of residuals x
##' @references Wilks, D.S. Statistical Methods in the Atmospheric
##' Sciences. 2nd edition, 2006.
##' @author Timothy W. Hilton
getLikelihood <- function(x, mu, sigma) {

  product <- (exp( -1 * (x - mu)^2 / (2 * sigma^2) ))#, na.rm=TRUE)
  return(product / (sigma * sqrt(2 * pi)))
}

#==============================================================
##' calculates sum of squares of X.
##'
##' if there are no valid data points, return an arbitrary (and very
##' high) SSE. This is a little bit of a hack - it really ought to
##' return NA in that situation, but it can"t return NA, because the
##' DEoptim implementation of DE requires that the optimized function
##' return a non-NA scalar.
##' @title calculate sum of squares
##' @param x values for which to calculate sum of squares
##' @return sum of squares of x
##' @author Timothy W. Hilton
getSSE <- function(x) {


  if (length(which(!is.na(x))) == 0) return(1e20)
  else return(sum( x^2, na.rm=TRUE))
}

##' calculate sum of absolute errors, ignoring NAs.
##'
##' if there are no valid (non-NA) data points, return a very high
##' value (currently 1e20).  This allows use with DEoptim, which
##' issues an error if a cost function returns NA.
##' @title calculate sum of absolute errors.
##' @param x error values
##' @return sum of absolute errors in x.
##' @author Timothy W. Hilton
getAbsWeightedErr <- function(x) {

  if (length(!is.na(x)) == 0) return(1e20)
  else return(sum(abs(x), na.rm=TRUE))
}

##' calculate sum of squared errors (SSE) for VPRM NEE, given a set of VPRM
##' parameters and a a set of NEE observations.
##'
##' "L" abbreviations likelihood; currently SSE is a standin for a statistically proper likelihood function for land surface model residuals.
##' @title cost function for VPRM parameterization
##' @param par four-element numeric vector containing lambda, alpha,
##' beta, PAR_0 (in that order)
##' @param driver_data May be a VPRM_driver_data object or a data
##' frame.  If a data frame, driver_data must contain the variables
##' Tscale, Pscale, Wscale, EVI, and PAR, and NEE_obs.
##' @return sum of squared errors for VPRM NEE residuals for the
##' specified parameters and NEE observations.
##' @author Timothy W. Hilton
"L" <- function(par, driver_data) {
    driver_data <- as.data.frame(driver_data)
    NEE_vprm <- vprm_calc_NEE(driver_data, par[1], par[2], par[3], par[4])
    res <- driver_data[['NEE_obs']] - NEE_vprm
    return(getSSE(res))
}

##' define VPRM parameter windows (upper and lower) and call DEoptim
##' to estimate VPRM parmeters.  Helper function for
##' estimate_VPRM_pars.
##'
##' @title run DEoptim to estimate VPRM parameter values
##' @param driver_data May be a VPRM_driver_data object or a data
##' frame.  If a data frame, driver_data must contain the variables
##' Tscale, Pscale, Wscale, EVI, and PAR, and NEE_obs.
##' @param DEcontrol DEoptim.control object
##' @param refresh how many DE iterations in between progress writeouts
##' @param msg character string; message to be displayed before DE is
##' called.  Useful for log files.
##' @param lambda_prior 2-element vector; upper and lower lambda
##' values for optimization.  Default is c(0.0, 1.5).
##' @param alpha_prior  2-element vector; upper and lower alpha
##' values for optimization.  Default is c(0.0, 1.5).
##' @param beta_prior  2-element vector; upper and lower beta
##' values for optimization.  Default is c(-4.0, 4.0).
##' @param PAR0_prior  2-element vector; upper and lower PAR_0
##' values for optimization.  Default is c(0.1, 6000).
##' @return DEoptim object. DEoptim output for best VPRM parameter
##' estimate.
##' @import DEoptim
##' @export
##' @author Timothy W. Hilton
##' @examples
##' data(Park_Falls)
optimizeVPRM_DE <- function(driver_data,
                            DEcontrol,
                            refresh=5,
                            msg='',
                            lambda_prior=c(0.0, 1.5),
                            alpha_prior=c(0.0, 1.5),
                            beta_prior=c(-4.0, 4.0),
                            PAR0_prior=c(0.1 , 6000)) {

    cat('within optimizeVPRM_DE:\n')
    cat('lambda_prior: ', lambda_prior, '\n')
    cat('alpha_prior: ', alpha_prior, '\n')
    cat('beta_prior: ', beta_prior, '\n')
    cat('PAR0_prior: ', PAR0_prior, '\n')

                                        #-----
    lower <- c(lambda_prior[1], alpha_prior[1], beta_prior[1], PAR0_prior[1])
    upper <- c(lambda_prior[2], alpha_prior[2], beta_prior[2], PAR0_prior[2])

    cat("upper: ", upper, "\n")
    cat("lower: ", lower, "\n")

    if ( nchar( msg )  > 0 )
        cat( msg )

    ##-----
    ##find optimized parameters

    r <- DEoptim(fn=L, lower=lower, upper=upper, control=DEcontrol, driver_data)

    ##-----
    ##do some bookkeeping
    ##give optimized parameters names (as opposed to 1,2,3,4)
    names(r[['optim']][['bestmem']]) <- c("lambda", "alpha", "beta", "PAR_0")

    return(r)
}

##' @title calculate temporal bin edges for VPRM parameter estimation.
##' @param data data frame; tower observations
##' @param window character string; currently one of "all" or one of
##' the "breaks" values accepted by cut.Date.
##' @param t.start chron; beginning of first temporal window
##' @param t.end chron; end of last temporal window
##' @return factor; the temporal bin of each timestamp in data
##' @import chron
##' @author Timothy W. Hilton
get_time_bins <- function(data, window, t.start, t.end) {
  if (is.numeric(window)) bin.edges <- as.Date(seq(t.start, t.end, by=window))
  else if (is.character(window)) bin.edges <- window  #window is a string

  if (window == "all")
    time.bins <- as.factor(rep("all", nrow(data)))
  else
    time.bins <- cut(as.Date(data$date), breaks=bin.edges)
  return(time.bins)
}



## evaluates a boolean function on each element from a list, removing
## the elements from the list that return false.
##
## bool.FUN defaults to !is.null; i.e. list.trim defaults to
## returning list.in with NULL elements removed
## @param list.in an R list
## @param bool.FUN any boolean function; defaults to not.null
## @return an R list containing only elements of list.in for which
## bool.FUN returned TRUE
## @author Timothy W. Hilton
list.trim <- function(list.in, bool.FUN=not.null) {
  idx <- unlist(lapply(list.in, bool.FUN))
  return(list.in[idx])
}

## returns true if its argument is not NULL
##
## @param x an R object
## @return TRUE if x is not NULL, FALSE if x is NULL
## @author Timothy W. Hilton
not.null <- function(x) {
    return(!is.null(x))
}
