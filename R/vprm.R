##' calculates Tscale according to eqn 6 in Mahadevan et al, 2007
##'
##' @title calculate Tscale
##' @param T T: 1xN array of observed temperatures (C)
##' @param Tmax maximum temp at which photosynthesis occurs (C)
##' @param Tmin minimum temp at which photosynthesis occurs (C)
##' @param Topt optimal temperature for photosynthesis (C)
##' @return Tscale term in VPRM equation (eqn 6 in Mahadevan et al,
##' 2007)
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @author Timothy W. Hilton
##' @export
##' @examples
##' data(Park_Falls)
##' Tscale <- getTscale( PFa_tower_obs[['TA']], Tmax=40, Tmin=0, Topt=20 )
getTscale <- function (T, Tmax, Tmin, Topt)
{
  numer <- (T - Tmin) * (T - Tmax)
  denom <- ((T - Tmin) * (T - Tmax)) - (T - Topt)^2
  Tscale = numer/denom
  Tscale[which(T < Tmin)] <- 0
  Tscale[which(T > Tmax)] <- 0
  Tscale[ Tscale < 0 ] <- 0
  Tscale[ Tscale > 1 ] <- 1
  return(Tscale)
}

##' calculates Pscale according to eqn 7 in Mahadevan et al, 2007
##'
##' The levels of phen are abbreviations: ginc - "onset greenness
##' increase", gmin - "onset greenness minimum", gmax - "onset
##' greenness maximum", gdec - "onset greenness decrease".  This
##' phenology dynamics classification scheme is explained more fully
##' in Zhang et al (2003).
##' @title calculate Pscale
##' @param LSWI 1xN numeric vector; land surface water index
##' @param phen 1xN factor vector; MODIS phenology (factor with levels
##' ginc, gmin, gmax, gdec)
##' @return Pscale term in VPRM equation (eqn 7 in Mahadevan et al, 2008)
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
##' @references Xiaoyang Zhang, Mark A. Friedl, Crystal B. Schaaf,
##' Alan H. Strahler, John C.F. Hodges, Feng Gao, Bradley C. Reed,
##' Alfredo Huete, Monitoring vegetation phenology using MODIS, Remote
##' Sensing of Environment, Volume 84, Issue 3, March 2003, Pages
##' 471-475, ISSN 0034-4257,
##' http://dx.doi.org/10.1016/S0034-4257(02)00135-9.
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
##' pfa_df <- as.data.frame( pfa_dd )
##' pscale <- getPscale( pfa_df[['LSWI']], pfa_df[['phen']] )
getPscale <- function( LSWI, phen ) {

  Pscale <- (1 + LSWI)/2
  Pscale[ phen == 'gmax' ] <- 1.0
  return( Pscale )

}

##' calculates Wscale according to eqn 8 in Mahadevan et al, 2007
##'
##' @title calculate Wscale
##' @param LSWI numeric vector; land surface water index
##' @param LSWI_max numeric; maximum LSWI for site (that is, a single value)
##' @return Wscale term in VPRM equation (eqn 8 in Mahadevan et al, 2007)
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
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
##' pfa_df <- as.data.frame( pfa_dd )
##' wscale <- getWscale(pfa_df[['LSWI']], max(pfa_df[['LSWI']], na.rm=TRUE ))
getWscale <- function(LSWI, LSWI_max) {
  Wscale <- (1 + LSWI) / (1 + LSWI_max)
  return(Wscale)
}

##' calculates land surface water index (LSWI) according to eqn 3 in
##' Mahadevan et al, 2007.
##'
##' LSWI may not exceed 1.0 and may take negative values (Xiao et al 2004)
##' @title calculate LSWI
##' @param rho_nir 1xN numeric vector; near-infrared (841-876 nm) band
##' satellite-derived reflectance
##' @param rho_swir 1xN numeric vector; 1628-1652 nm band
##' satellite-derived reflectance
##' @return LSWI: numeric vector same size as rho_nir and rho_swir
##' containing LSWI values.
##' @author Timothy W. Hilton
##' @references Xiangming Xiao, Qingyuan Zhang, Bobby Braswell, Shawn
##' Urbanski, Stephen Boles, Steven Wofsy, Berrien Moore III, Dennis
##' Ojima, Modeling gross primary production of temperate deciduous
##' broadleaf forest using satellite images and climate data, Remote
##' Sensing of Environment, Volume 91, Issue 2, 30 May 2004, Pages
##' 256-270, ISSN 0034-4257, 10.1016/j.rse.2004.03.010.
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
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
##' pfa_df <- as.data.frame( pfa_dd )
##' LSWI <- getLSWI( rho_nir=pfa_df[['rho_nir']],
##'                  rho_swir=pfa_df[['rho_swir']] )
getLSWI <- function(rho_nir, rho_swir) {
  LSWI <- (rho_nir - rho_swir) / (rho_nir + rho_swir)
  return(LSWI)
}


##' calculates VPRM net ecosystem exchange (NEE) according to eqn 12
##' in Mahadevan et al, 2007
##'
##' Arguments lambda, alpha, beta, and PAR_0 may be omitted from the
##' function call.  In this case they must be present as variables in
##' data frame driver_data.  If any of these four parameters are present in
##' driver_data *and* specified as parameters in the function call the
##' function parameter values will be used and the values in driver_data
##' will be ignored.  If specifed as function parameters lambda,
##' alpha, beta, and PAR_0 may be single values or numeric vectors the
##' same length as the number of observations in driver_data.
##'
##' The Tresp variable in driver_data is the temperature used to calculate
##' respiration.  Tresp should be max(Tair, Tlow), where Tair is the
##' air temperature (deg C) and Tlow is the minimum air temperature
##' (deg C) for respiration.  This is explained more fully in
##' Mahadevan et al (2008) section 2.2.
##' @title calculate VPRM NEE
##' @param driver_data May be a VPRM_driver_data object or a data
##' frame.  If a data frame, driver_data must contain the variables
##' Tscale, Pscale, Wscale, EVI, PAR, and Tresp.  The variables
##' lambda, alpha, beta, and PAR_0 are optional (see 'details').
##' @param lambda_param numeric, optional; VPRM parameter: maximum light use
##' efficiency.
##' @param alpha_param numeric, optional; VPRM parameter (slope of respiration with
##' respect to temperature)
##' @param beta_param numeric, optional; VPRM parameter (basal respiration rate)
##' @param PAR_0_param numeric, optional; VPRM parameter (LUE half-saturation value)
##' @return vector of same length as number of rows in driver_data containin
##' VPRM NEE [umol m-2 s-1]
##' @author Timothy W. Hilton
##' @references Hilton, T. W., Davis, K. J., Keller, K., and Urban,
##' N. M.: Improving North American terrestrial CO2 flux diagnosis
##' using spatial structure in land surface model residuals,
##' Biogeosciences, 10, 4607-4625, doi:10.5194/bg-10-4607-2013, 2013.
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
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
##' data(VPRM_parameters)
##' attach(all_all_VPRM_parameters)
##' NEE <- vprm_calc_NEE(pfa_dd,
##'                      lambda=lambda, PAR_0=PAR_0, alpha=alpha, beta=beta)
vprm_calc_NEE <- function(driver_data, lambda_param=NULL, alpha_param=NULL, beta_param=NULL, PAR_0_param=NULL) {
    GEE <- vprm_calc_GEE(driver_data, lambda_param, PAR_0_param)
    R <- vprm_calc_R(driver_data, alpha_param, beta_param)
    NEE <- R - GEE
    return(NEE)
}

##' calculate VPRM GEE according to Mahadevan et al (2008) eq. 9
##'
##' Arguments lambda and PAR_0 may be omitted from the function call.
##' In this case they must be present as variables in data frame
##' driver_data.  If either of these parameters are present in
##' driver_data *and* specified as parameters in the function call the
##' function parameter values will be used and the values in
##' driver_data will be ignored.  If specifed as function parameters
##' lambda and PAR_0 may be single values or numeric vectors the same
##' length as the number of observations in driver_data.
##'
##' @title calculate VPRM GEE
##' @param driver_data May be a VPRM_driver_data object or a data
##' frame.  If a data frame, driver_data must contain the variables
##' Tscale, Pscale, Wscale, EVI, and PAR.  The variables
##' lambda, and PAR_0 are optional (see 'details').
##' @param lambda_param numeric, optional; VPRM parameter: maximum
##' light use efficiency.
##' @param PAR_0_param numeric, optional; VPRM parameter (LUE
##' half-saturation value)
##' @return vector of same length as number of rows in driver_data containin
##' VPRM GEE [umol m-2 s-1]
##' @author Timothy W. Hilton
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn,
##' A., Lin, J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A
##' satellite-based biosphere parameterization for net ecosystem CO2
##' exchange: Vegetation Photosynthesis and Respiration Model
##' (VPRM), Global Biogeochem. Cy., 22, GB2005,
##' doi:10.1029/2006GB002735, 2008.
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
##' data(VPRM_parameters)
##' attach(all_all_VPRM_parameters)
##' GEE <- vprm_calc_GEE(pfa_dd, lambda=lambda, PAR_0=PAR_0)
vprm_calc_GEE <- function(driver_data, lambda_param=NULL, PAR_0_param=NULL) {

    driver_data <- as.data.frame( driver_data )

    ## make sure VPRM parameters were specified as either function
    ## parameters or within driver_data
    if (is.null(lambda_param)) {
        if ('lambda' %in% names( driver_data ) ) {
            lambda_param <- driver_data[['lambda']]
        } else {
            stop('lambda is unspecified')
        }
    }
    if (is.null(PAR_0_param)) {
        if ('PAR_0' %in% names( driver_data ) ) {
            PAR_0_param <- driver_data[['PAR_0']]
        } else {
            stop('PAR_0 is unspecified')
        }
    }

    ## calculate GEE according to Mahadevean et al (2008) eqn 9
    GEE <- (lambda_param *
            driver_data[, "Tscale"] *
            driver_data[, "Pscale"] *
            driver_data[, "Wscale"] *
            (1 / (1 + (driver_data[, "PAR"]/PAR_0_param))) *
            driver_data[, "EVI"] *
            driver_data[, "PAR"] )

    return(GEE)
}


##' calculates VPRM ecosystem respiration (R) according to either the VPRM
##' formulation of Mahadevan et al. (2007) eqn 10 or the urbanVPRM
##' formulation of Hardiman et al (2017) SI eqn 7 and SI eqn 8.
##'
##' Arguments alpha and beta may be omitted from the function call.
##' In this case they must be present as variables in data frame
##' driver_data.  If either of these parameters are present in
##' driver_data *and* specified as parameters in the function call the
##' function parameter values will be used and the values in
##' driver_data will be ignored.  If specifed as function parameters
##' alpha and beta may be single values or numeric vectors the same
##' length as the number of observations in driver_data.
##'
##' The Tresp variable in driver_data is the temperature used to calculate
##' respiration. Tresp should be max(Tair, Tlow), where Tair is the air
##' temperature (deg C) and Tlow is the minimum air temperature (deg C) for
##' respiration. This is explained more fully in Mahadevan et al (2008) section
##' 2.2.
##'
##' The urbanVPRM subdivides respiration into heterotrophic and autotrophic and
##' introduces a scaling factor based on a nearby pixel of similar land cover
##' with minimal impervious surface area. This scaling factor is derived from a
##' "reference EVI" term, described in more detail in the supplemental material
##' of Hardiman et al. (2017).
##' @title calculate VPRM ecosystem respiration
##' @param driver_data May be a VPRM_driver_data object or a data frame. If a
##'   data frame, driver_data must contain the variables Tscale, Pscale, Wscale,
##'   EVI, PAR, and Tresp. The variables alpha and beta are optional (see
##'   'details').
##' @param alpha_param numeric, optional; VPRM parameter (slope of respiration
##'   with respect to temperature)
##' @param beta_param numeric, optional; VPRM parameter (basal respiration rate)
##' @param model_form string, optional; form of VPRM model to use. Options are
##'   "Mahadevan07" (default) to use the VPRM formulation of Mahadevan et al.
##'   (2007), or "urban" to use the urbanVPRM formulation of Hardiman et al.
##'   (2017). If set to "urban", the driver data must include variables ISA
##'   proportion (impervious surface area, 0.0 to 1.0) and refEVI (reference
##'   EVI).
##' @return vector of same length as number of rows in driver_data containin
##'   VPRM ecosystem respiration [umol m-2 s-1]
##' @author Timothy W. Hilton
##' @references Hilton, T. W., Davis, K. J., Keller, K., and Urban, N. M.:
##'   Improving North American terrestrial CO2 flux diagnosis using spatial
##'   structure in land surface model residuals, Biogeosciences, 10, 4607-4625,
##'   doi:10.5194/bg-10-4607-2013, 2013.
##' @references Mahadevan, P., Wofsy, S., Matross, D., Xiao, X., Dunn, A., Lin,
##'   J., Gerbig, C., Munger, J., Chow, V., and Gottlieb, E.: A satellite-based
##'   biosphere parameterization for net ecosystem CO2 exchange: Vegetation
##'   Photosynthesis and Respiration Model (VPRM), Global Biogeochem. Cy., 22,
##'   GB2005, doi:10.1029/2006GB002735, 2008.
##' @references Hardiman, B. S., Wang, J. A., Hutyra, L. R., Gately, C. K.,
##'   Getson, J. M., & Friedl, M. A. (2017). Accounting for urban biogenic
##'   fluxes in regional carbon budgets. Science of The Total Environment, 592,
##'   366–372. https://doi.org/10.1016/j.scitotenv.2017.03.028
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
##'
##' data(VPRM_parameters)
##' attach(all_all_VPRM_parameters)
##' ER <- vprm_calc_R(pfa_dd, alpha=alpha, beta=beta)
vprm_calc_R <- function(driver_data,
                        alpha_param=NULL,
                        beta_param=NULL,
                        model_form='Mahadevan07') {

  driver_data <- as.data.frame( driver_data )

  ## make sure VPRM parameters were specified as either function
  ## parameters or within driver_data
  if (is.null(alpha_param)) {
    if ('alpha' %in% names( driver_data ) ) {
      alpha_param <- driver_data[['alpha']]
    } else {
      stop('alpha is unspecified')
    }
  }
  if (is.null(beta_param)) {
    if ('beta' %in% names( driver_data ) ) {
      beta_param <- driver_data[['beta']]
    } else {
      stop('beta is unspecified')
    }
  }
  # calculate R for Mahadevan et al 2007 VPRM formulation
  R <- alpha_param * driver_data[, "Tresp"] + beta_param

  if (model_form == 'urban') {
    R <- urbanvprm_calc_R(R, driver_data, alpha_param, beta_param)
  }

  return(R)
}

##' calculates urbanVPRM ecosystem respiration (R) according to Hardiman et al.
##' (2017) SI eqn 7 and SI eqn 8 as described in SI section S2.4.
##'
##' .. content for \details{} ..
##' @title calculate urbanVPRM ecosystem respiration
##' @param Rinit numeric; the Rinit term, equal to the respiration for the
##'   Mahadevan et al (2007) formulation.
##' @inherit vprm_calc_R return author references params
##' @param alpha_param numeric, optional; VPRM parameter (slope of respiration
##'   with respect to temperature)
##' @param beta_param numeric, optional; VPRM parameter (basal respiration rate)
##' @param model_form string, optional; form of VPRM model to use. Options are
##'   "Mahadevan07" (default) to use the VPRM formulation of Mahadevan et al.
##'   (2007), or "urban" to use the urbanVPRM formulation of Hardiman et al.
##'   (2017). If set to "urban", the driver data must include variables ISA
##'   proportion (impervious surface area, 0.0 to 1.0) and refEVI (reference
##'   EVI).
##' @return
##' @author Timothy W. Hilton
urbanvprm_calc_R <- function(Rinit, driver_data, alpha_param, beta_param) {

  if (!('ISA' %in% names( driver_data ) )) {
    stop('ISA is unspecified')
  }

  if (!('refEVI' %in% names( driver_data ) )) {
    stop('reference EVI is unspecified')
  }

  ## heterotrophic respiration; Hardiman et al 2017 SI eqn 7
  Rh <- ((1.0 - driver_data[['ISA']]) * Rinit) / 2.0

  ## autotrophic respiration; Hardiman et al 2017 SI eqn 8
  Ra <- (((driver_data[['EVI']] +
          (min(driver_data[['refEVI']]) * driver_data[['ISA']])) /
         driver_data[['refEVI']]) *
         Rinit) / 2.0
  ## Hardiman et al assume that autotrophic and heterotrophic respiration
  ## contribute equally to ecosytem respiration (see SI section S2.4, first
  ## paragraph).
  return(Rh + Ra)
}
