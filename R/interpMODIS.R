##' interpolate MODIS band data to provide values at a given set of
##' dates using either linear or constant interpolation.
##'
##' @title interpolate MODIS band data to provide values at a given set of dates
##' @param mod_date chron vector; MODIS Time stamp
##' @param mod_val MODIS values
##' @param out_date chron vector; the dates for the output
##' @param method string, either "linear" or "constant"
##' @return list with components "date" and "val", containing
##' timestamps (class chron) and values (class is dependent on the
##' MODIS product) of the interpolated data.
##' @author Timothy W. Hilton
##' @import chron
##' @export
interpMODIS <- function(mod_date, mod_val, out_date, method) {

    ## if the phenology is NULL, return a data frame with all of the
    ## requested dates and with NA in the phenology column
    if ( is.null( mod_val ) ) {
        return( data.frame( date=out_date, val=as.factor(NA) ) )
    }

    if ( is.factor( mod_val ) )
        ## make a dictionary to map its factor labels to numeric values so
        ## that interpolated values can be mapped back to a factor
        idx_dict <- data.frame( labels=levels( mod_val ),
                               idx=1:length( levels( mod_val ) ) )

    ## approx fails if mod_val is all NA -- handle that case here
    if ( all( is.na( mod_val ) ) ) {
        out <- list( date=out_date,
                    val=rep( NA, length.out=length( out_date ) ) )
    } else {
        ##  do the interpolation
        out <- approx(mod_date, mod_val, xout=out_date, method=method)
    }
    names(out) <- c("date", "val")

    if ( is.factor( mod_val ) ) {
        ## remap a interpolated values (now numeric) back to a factor
        out$val <- idx_dict[ match( out$val, idx_dict$idx ), 'labels' ]
        out$val <- factor( out$val, ordered = TRUE )
    }
    
    return(out)
}
