##' perform constant interpolation of phenology factor codes( ginc,
##' gmax, gdec, gmin ). That is, fill phenology transition status
##' forward until the value changes, resulting in a ginc, gmax, gdec,
##' or gmin value at all timestamps.
##'
##' The interpolation converts the factors to
##' numeric codes 1 to 4 -- this code creates a dictionary and maps
##' them back, preserving their factor status for readability.  
##'
##' @title fill MODIS phenology status to arbitrary dates.
##' @param phen data frame with variables date (chron), phen (factor
##' with levels ginc, gdec, gmin, gmax)
##' @param dates chron; dates to fill in. 
##' @return phen filled to the dates in dates
##' @author Timothy W. Hilton
##' @export
##' @examples
##' data(Park_Falls)
##' interpolated_phen <- interp_phenology(phen=PFa_phen, 
##'                                       dates=PFa_tower_obs[['date']])
interp_phenology <- function( phen, dates ) {

  idx_dict <- data.frame( code=levels( phen$phen ),
                         idx=1:length( levels( phen$phen ) ) )

  i_phen <- interpMODIS( phen$date, phen$phen, dates, 'constant' )
  i_phen <- as.data.frame( i_phen )
  names( i_phen ) <- gsub( 'val', 'phen', names( i_phen ) )
  return( i_phen )
}


