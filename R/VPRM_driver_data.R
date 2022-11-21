## implements S3 class VPRM_driver_data, which assembles all the data
## necessary to estimate VPRM parameters.

##' class constructor for VPRM_driver_data.  Accepts all driver data
##' necessary to run VPRM for a single eddy covariance site,
##' calculates "derived" fields, and interpolates phenology dynamics.
##'
##' "Derived fields" denote fields that are calculated from other
##' observed quanitities.  For example, land surface water index
##' (LSWI) is a derived field, as it is calculated from MODIS
##' reflectances in the short infrared and near infrared bands.
##' @title build a VPRM_driver_data object
##' @param name_long character string; "short name" of the site.  e.g. US-PFa
##' @param name_short character string; "long name" of the site.  e.g. Park Falls
##' @param lat numeric; latitude of the site (deg N)
##' @param lon  numeric; longitude of the site (deg E)
##' @param PFT character string; plant functional type.  Will be converted to a factor.
##' @param note character string; optional note; could be anything the
##' user finds useful.
##' @param Tmin numeric; minimum temperature for photosynthesis (deg
##' C).  See Mahadevan et al (2008) eq. 6.
##' @param Tmax  numeric; maximim temperature for photosynthesis (deg
##' C).  See Mahadevan et al (2008) eq. 6.
##' @param Topt  numeric; optimum temperature for photosynthesis (deg
##' C).  See Mahadevan et al (2008) eq. 6.
##' @param Tlow numeric; minimum temperature for respiration (deg C).
##' See Mahadevan et al (2008) eq. 10.
##' @param tower_date chron vector; timestamps for all tower
##' observations (NEE_obs, T, PAR)
##' @param NEE_obs numeric vector; eddy covariance observed net
##' ecosystem exchange (NEE, umol m-2 s-1)
##' @param T numeric vector; observed air temperature (deg C)
##' @param PAR numeric vector; observed photosynthetically active
##' radiation (umol m-2 s-1)
##' @param date_nir chron vector; timestamps for NIR reflectance.
##' @param rho_nir numeric vector; NIR reflectance values
##' @param date_swir  chron vector; timestamps for SWIR reflectance.
##' @param rho_swir  numeric vector; SWIR reflectance values
##' @param date_EVI  chron vector; timestamps for enhanced vegetation index (EVI).
##' @param EVI numeric vector; EVI values.
##' @param phen factor; phenology dynamics.  levels are ginc (onset
##' greenness increase), gdec (onset greenness decrease), gmin (onset
##' greenness minimum), gmax (onset greenness maximum).  If not
##' specified, phenology dynsamics are calculated from EVI using a
##' method similar to Zhang et al (2003).
##' @return an object of class VPRM_driver_data.  Has fields (see
##' above arguments for definitions): name_long, name_short, lat, lon,
##' PFT, note, Tmin, Tmax, Topt, Tlow.  The data for NEE_obs, T, PAR,
##' rho_nir, rho_swir, EVI, and phen should be accessed using the
##' as.data.frame method.
##' @references Mahadevan, P., S. C. Wofsy, D. M. Matross, X. Xiao,
##' A. L. Dunn, J. C. Lin, C. Gerbig, J. W. Munger, V. Y. Chow, and
##' E. W. Gottlieb (2008), A satellite-based biosphere
##' parameterization for net ecosystem CO2 exchange: Vegetation
##' Photosynthesis and Respiration Model (VPRM), Global
##' Biogeochem. Cycles, 22, GB2005, doi:10.1029/2006GB002735.
##' @references Xiaoyang Zhang, Mark A. Friedl, Crystal B. Schaaf,
##' Alan H. Strahler, John C.F. Hodges, Feng Gao, Bradley C. Reed,
##' Alfredo Huete, Monitoring vegetation phenology using MODIS, Remote
##' Sensing of Environment, Volume 84, Issue 3, March 2003, Pages
##' 471-475, ISSN 0034-4257,
##' http://dx.doi.org/10.1016/S0034-4257(02)00135-9.
##' @author Timothy W. Hilton
##' @import chron
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
##' print(head(as.data.frame(pfa_dd)))

VPRM_driver_data <- function( name_long="",
                        name_short="",
                        lat=NA,
                        lon=NA,
                        PFT=NA,
                        note="",
                        ## scalars
                        Tmin=0, #minimum respiration temperature (C)
                        Tmax=40, #maximum respiration temperature (C)
                        Topt=20, #optimal respiration temperature (C)
                        Tlow=2,
                        ## ---
                        ## observed fields
                        tower_date=NA,
                        NEE_obs=NA,
                        T=NA,
                        PAR=NA,
                        date_nir=NA,
                        rho_nir=NA,
                        date_swir,
                        rho_swir=NA,
                        date_EVI,
                        EVI=NA,
                        refEVI=NA,  # needed only for urbanVPRM
                        ISA=NA,     # needed only for urbanVPRM
                        LSWI=NA,    # if not specified will be calculated from rho_nir, rho_swir
                        ## phen should be a data frame with columns "date" and "phen"
                        phen=NA ) {

## all the 'scalar' fields; that is, fields that are single values, not time
## series
obj <- list( name_long=name_long,
            name_short=name_short,
            lat=lat,
            lon=lon,
            note=note,
            PFT=PFT,
            Tmin=Tmin,
            Tmax=Tmax,
            Topt=Topt )

## interpolate reflectances and EVI onto tower timestamps
EVI <- interpMODIS( date_EVI, EVI, tower_date, "linear" )[['val']]
rho_nir <- interpMODIS( date_nir, rho_nir, tower_date, "linear" )[['val']]
rho_swir <- interpMODIS( date_swir, rho_swir, tower_date, "linear" )[['val']]
## if phen contains only NA, then calculate phenology transition dates
## from EVI
if ( identical( all( is.na( phen ) ), TRUE ) ) {
    EVI_df <- data.frame(sitecode=name_short,
                         t=tower_date,
                         EVI=EVI)
    phen <- detect_large_greenness_change_periods( EVI_df)
}
## interpolate phenology status (greenness increasing, greenness max,
## greenness decreasing, greenness minimum) onto tower dates
phen <- interp_phenology( phen, tower_date )

## try/catch here!
obj[['data']] <- data.frame( date=tower_date,
                       NEE_obs=NEE_obs,
                       T=T,
                       PAR=PAR,
                       rho_nir=rho_nir,
                       rho_swir=rho_swir,
                       EVI=EVI,
                       refEVI=refEVI,
                       ISA=ISA,
                       LSWI=LSWI,
                       phen=phen[['phen']] )

class( obj ) <- c( 'VPRM_driver_data', class( obj ) )

obj <- calculate_VPRM_derived_input_fields( obj )

return( obj )

}

### ------------------------------------------------------------

## calculate LSWI, Tscale, Pscale, Wscale, and Tresp from observed
## quantities.
##
## This is a helper function for VPRM_driver_data, and is not public.
## @title calculate derived VPRM inputs from observations
## @param obj VPRM_driver_data object; provides the observations necessary.
## @return VPRM_driver_data object; same as input argument obj, but with
## derived fields filled in.
## @author Timothy W. Hilton
calculate_VPRM_derived_input_fields <- function( obj ) {

  ## make sure class of obj is correct
  if (!('VPRM_driver_data' %in% class(obj)))
    stop('obj must be a VPRM_driver_data object')


  if (identical(all(is.na(obj[['data']][['LSWI']])), TRUE)) {
    obj[['data']][['LSWI']] <- with(obj[['data']],
                                    getLSWI(rho_nir, rho_swir))
  }
  obj[['data']][['Tscale']] <- getTscale(obj[['data']][['T']],
                                         obj[['Tmax']],
                                         obj[['Tmin']],
                                         obj[['Topt']])
  if (any(!is.na(obj[['data']][['phen']]))) {
    ## for non-evergreen classes, derive Pscale from phenology
    obj[['data']][['Pscale']] <- with(obj[['data']], getPscale(LSWI, phen))
  } else {
    ## for evergreen classes, set Pscale to 1.0 (Mahadevan et al, 2008)
    obj[['data']][['Pscale']] <- 1.0
  }
  
  obj[['data']][['Wscale']] <- with(obj[['data']],
                                    getWscale(LSWI, max(LSWI, na.rm=TRUE)))

  Tresp <- obj[['data']][['T']]
  Tresp[ Tresp < obj[['Tlow']] ] <- obj[['Tlow']]
  obj[['data']][['Tresp']] <- Tresp
  
  return( obj )
}


##' method for VPRM_driver_data
##'
##' The data field of the VPRM_driver_data object is kept.  Other
##' fields (short name, long name, etc.) are discarded.
##' @title coerce to a data frame
##' @param x VPRM_driver_data object
##' @param ... additional arguments to be passed to or from methods.
##' @return data frame with fields variables NEE_obs, T, PAR, rho_nir,
##' rho_swir, EVI, and phen.
##' @method as.data.frame VPRM_driver_data
##' @author Timothy W. Hilton
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
##' print(head(as.data.frame(pfa_dd)))
as.data.frame.VPRM_driver_data <- function( x, ... ) {
  return( x[[ 'data' ]] )
}

