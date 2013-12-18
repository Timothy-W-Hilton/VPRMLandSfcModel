##' locate periods of sustained greeness increase or decrease within
##' an EVI time series
##'
##' @title implememnts an approximation of the methods of Zhang et al
##' (2003, 2006).
##' @param data data frame with (at least) columns "sitecode" (unique
##' sitecode), "t" (chron object timestamps) and "EVI" (EVI
##' observations). Data should contain EVI observations from one and
##' only one site.
##' @param verbose integer; causes silent execution if 0 (default);
##' larger values cause increasingly verbose printouts.  Currently has
##' two options: silent if 0; prints site being processed if >0; this
##' may change in the future to provide more granularity.
##' @return data frame with columns "idx" (row indices of data where
##' slope of EVI change changes sign), "date" (chron object containing
##' the timestamps corresponding to idx), and "phen" (factor encoding
##' the type of EVI change, with levels ginc, gdec, gmin, gmax.
##' @author Timothy W. Hilton
##' @references Xiaoyang Zhang, Mark A. Friedl, Crystal B. Schaaf,
##' Alan H. Strahler, John C.F. Hodges, Feng Gao, Bradley C. Reed,
##' Alfredo Huete, Monitoring vegetation phenology using MODIS, Remote
##' Sensing of Environment, Volume 84, Issue 3, March 2003, Pages
##' 471-475, ISSN 0034-4257,
##' http://dx.doi.org/10.1016/S0034-4257(02)00135-9.
##' @references M. A. Friedl, and C. B. Schaaf (2006), Global
##' vegetation phenology from Moderate Resolution Imaging
##' Spectroradiometer (MODIS): Evaluation of global patterns and
##' comparison with in situ measurements, J. Geophys. Res., 111,
##' G04017, doi:10.1029/2006JG000217.
##' @export
##' @examples
##' data(Park_Falls)
##' names(PFa_evi) <- c('t', 'EVI', 'sitecode')
##' PFa_evi[['sitecode']] <- "US-PFa"
##' print(head(PFa_evi))
##' phen_transition_dates <- detect_large_greenness_change_periods(PFa_evi)
##' print(head(phen_transition_dates))
detect_large_greenness_change_periods <- function( data, verbose=0 ) {

    ## data should contain observations from one and only one site
    n_sites <- length( unique( data[[ 'sitecode' ]] ) )
    if ( n_sites > 1 ) {
        stop( 'more than one site' )
    }

    ## return empty data frame if there are no valid EVI observations
    valid_EVI_data <- any( !is.na( data[[ 'EVI' ]] ) )
    if ( !valid_EVI_data ) {
        return( data.frame( NULL ) )
    }

    if ( verbose > 0 ) {
        print( as.character (data[ 1, 'sitecode' ] ) )
    }

    annual_range <- diff( range( data[[ 'EVI' ]], na.rm=TRUE ) )
    annual_max <- max( data[[ 'EVI' ]], na.rm=TRUE )

    delta <- diff( sign( diff( data[[ 'EVI' ]] ) ) )

    ## identify row indices in EVI time series where the sign of the
    ## slope of EVI changes.
    idx_delta <- data.frame( idx=which( delta != 0 ), phen=NA, keep=FALSE )
    ## separate slope changes into ginc (onset of Greenness INCrease)
    ## and gdec (onset of Greeness DECrease)
    idx_delta[[ 'phen' ]][ which( delta[ idx_delta[['idx']] ] > 0 ) ] <- 'ginc'
    idx_delta[[ 'phen' ]][ which( delta[ idx_delta[['idx']] ] < 0 ) ] <- 'gdec'
    idx_delta <- within( idx_delta, phen <-
                        factor( phen,
                               levels=c( 'gdec', 'gmax', 'gmin', 'ginc' ) ) )

    ## if there's only one greeness point of inflection then there is
    ## nothing to do
    n_inflection_pts <- nrow( idx_delta )
    if ( n_inflection_pts > 1 ) {

        ## as per Zhang et al (2006) section 3.2 "Vegetation growth and
        ## senescence are identified by periods of sustained EVI increase
        ## and decrease, respectively".  Identify those periods here.
        for( i in 1:(nrow( idx_delta ) - 1) ) {
            if ( idx_delta[ i, 'phen' ] == 'ginc' ) {
                if ( idx_delta[ i+1, 'phen' ] != 'gdec' ) {
                    warning( paste0( 'ginc not followed by gdec, idx: ', i ) )
                }
            }
            ## calculate the total EVI change across the entire
            ## period of increaes or decrease, then filter out
            ## small changes that are random fluctation, not onset
            ## of growth or senescence
            this_diff <- ( data[ idx_delta[ i+1, 'idx' ], 'EVI' ] -
                          data[ idx_delta[ i, 'idx' ], 'EVI' ] )
            ## ------
            ## "heuristic 1" from Zhang et al 2006, section 3.2
            h1 <- abs( this_diff ) > ( 0.35 * annual_range )
            ## ------
            ## "heuristic 2" from Zhang et al 2006, section 3.2
            ## find start & end of this growth or senesce period
            this_pd_bounds <- idx_delta[ c( i, i+1 ), 'idx' ] 
            this_pd_max_EVI <- max( data[ this_pd_bounds, 'EVI' ] )
            h2 <- this_pd_max_EVI > ( 0.7 * annual_max )
            if ( h1 & h2 ) {
                idx_delta[ i, 'keep' ] <- TRUE
                idx_delta[ i + 1, 'keep' ] <- TRUE
            }

        }
    }

    idx_delta <- idx_delta[ which( idx_delta[[ 'keep' ]] ), c( 'idx', 'phen' ) ]

    if ( nrow( idx_delta ) > 0 ) {
        idx_delta[[ 'date' ]] <- data[ idx_delta[[ 'idx' ]], 't' ]

        ## for two consecutive gdec, the first is a greenness minimum
        ## for two consecutive ginc, the first is a greenness maximim
        for( i in 1:( nrow( idx_delta ) - 1 ) ) {
            this_phen <- idx_delta[ i, 'phen' ]
            next_phen <- idx_delta[ i + 1, 'phen' ]
            if ( ( this_phen == 'gdec' ) & ( next_phen == 'gdec' ) ) {
                idx_delta[ i, 'phen' ] <- 'gmax'
            } else if ( ( this_phen == 'ginc' ) & ( next_phen == 'ginc' ) ) {
                idx_delta[ i, 'phen' ] <- 'gmin'
            }
        }
    }

    if ( nrow( idx_delta ) == 0 ) {
        idx_delta <- data.frame( NULL )
    }
    return( idx_delta )
}

