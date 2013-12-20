##' Park Falls, Wisconsin, USA eddy covariance data and MODIS data
##'
##' The dataset Park_Falls contains MODIS observations as well as
##' ground-based eddy covariance flux tower data for the Ameriflux
##' tall tower located in Park Falls, Wisconsin, USA.  The data span
##' the period 1 May 2005 through 31 July 2005.
##' @name Park_Falls
##' @docType data
##' @format The dataset Park_Falls contains four data frames:
##' \describe{
##' \item{PFa_evi}{timestamped MODIS EVI from NASA's Terra satellite (MODIS product MOD13Q1).}
##' \item{PFa_refl}{timestamped MODIS surface reflectances in the red, blue, near infrared (NIR), and shortwave infrared (SWIR) bands (MODIS product MCD43A4).}
##' \item{PFa_phen}{timestamped MODIS phenology dynamics data (MODIS product MOD12Q2)}
##' \item{PFa_tower_obs}{timestamped air temperature (TA), net ecosystem exchange of carbon dioxide (FC), and photosynthetically active radiation (PAR) data.}
##' }
##' @source The data in PFa_tower_obs are from the Park Falls,
##' Wisconsin Ameriflux level II dataset (\url{ameriflux.lbl.gov}).
##' MODIS data are from the Ameriflux site MODIS snapshots provided by
##' Oak Ridge National Laboratory (\url{http://daac.ornl.gov/MODIS/}).
##' @author Timothy W. Hilton
##' @seealso \code{\link{PFa_tower_obs}}, \code{\link{PFa_evi}},
##' \code{\link{PFa_refl}}, \code{\link{PFa_phen}}
##' @references MODIS snapshots: \url{http://daac.ornl.gov/MODIS/}
##' @references The Park Falls, Wisconsin, USA Ameriflux site:
##' \url{ameriflux.lbl.gov}, Berger et al (2000).
##' @references Berger, Bradford W., Kenneth J. Davis, Chuixiang Yi,
##' Peter S. Bakwin, Cong Long Zhao, 2001: Long-Term Carbon Dioxide
##' Fluxes from a Very Tall Tower in a Northern Forest: Flux
##' Measurement Methodology. J. Atmos. Oceanic Technol., 18, 529-542.
##' @examples
##' data(Park_Falls)
##' with( PFa_tower_obs, plot(date, TA,
##'                   ylab='T (C)', xlab='date',
##'                   main='Park Falls surface temperature'))
##' with( PFa_evi, plot(date, evi,
##'                     ylab='MODIS EVI', xlab='date',
##'                     main='Park Falls EVI' ))
NULL

##' Park Falls CO2 flux, PAR, and surface temperature
##'
##' @rdname PFa_tower_obs
##' @name PFa_tower_obs
##' @docType data
##' @format data frame containing ground-based observations from the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
##' \describe{
##' \item{date}{chron objects; the data timestamps}
##' \item{TA}{surface air temperature (C)}
##' \item{FC}{flux of CO2 (umol m-2 s-1)}
##' \item{PAR}{photosynthetically active radiation (umol m-2 s-1)}}
##' @source These data were extracted from the 2005 level II Ameriflux
##' file for US-PFa on on 16 Dec 2013.
##' @note PFa_tower_obs is part of the Park_Falls dataset. See
##' documentation for 'Park_Falls' for further details.
##' @seealso \code{\link{Park_Falls}}
##' @references \url{http://ameriflux.lbl.gov}
##' @examples
##' data(Park_Falls)
##' with( PFa_tower_obs, plot(date, TA,
##'                   ylab='T (C)', xlab='date',
##'                   main='Park Falls surface temperature'))
NULL

##' Park Falls EVI data
##'
##' @rdname PFa_evi
##' @name PFa_evi
##' @docType data
##' @format data frame containing MODIS enhanced vegetation index (EVI) for the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
##' \describe{
##' \item{date}{chron object; EVI timestamps (date and time)}
##' \item{evi}{the EVI value (unitless; 0 <= EVI <= 1)}
##' \item{bird}{the satellite that made the measurements; (terra|aqua)}
##' }
##' @note PFa_evi is part of the Park_Falls dataset. See documentation
##' for 'Park_Falls' for further details.
##' @source These data are extracted from the Oak Ridge National Laboratory MODIS subsets for MODIS product MOD13Q1.
##' @seealso \code{\link{Park_Falls}}
##' @references Oak Ridge National Laboratory Distributed Active Archive Center (ORNL DAAC). 2011. MODIS subsetted land products, Collection 5. Available on-line [http://daac.ornl.gov/MODIS/modis.html] from ORNL DAAC, Oak Ridge, Tennessee, U.S.A. Accessed 17 Dec, 2013.
##' @examples
##' data(Park_Falls)
##' with( PFa_evi, plot(date, evi,
##'                     ylab='MODIS EVI', xlab='date',
##'                     main='Park Falls EVI' ))
NULL


##' Park Falls phenology transition data
##'
##' @rdname PFa_phen
##' @name PFa_phen
##' @docType data
##' @format data frame containing MODIS enhanced vegetation index (EVI) for the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
##' \describe{
##' \item{date}{chron object; timestamps (date and time)}
##' \item{phen}{factor; phenology dynamics.  levels are ginc (onset greenness increase), gdec (onset greenness decrease), gmin (onset greenness minimum), gmax (onset greenness maximum).}
##' }
##' @note PFa_phen is part of the Park_Falls dataset. See
##' documentation for 'Park_Falls' for further details.
##' @source These data are extracted from the Oak Ridge National Laboratory MODIS subsets for MODIS product MOD12Q2.
##' @seealso \code{\link{Park_Falls}}
##' @references Oak Ridge National Laboratory Distributed Active Archive Center (ORNL DAAC). 2011. MODIS subsetted land products, Collection 5. Available on-line [http://daac.ornl.gov/MODIS/modis.html] from ORNL DAAC, Oak Ridge, Tennessee, U.S.A. Accessed 17 Dec, 2013.
##' @examples
##' data(Park_Falls)
##' print(PFa_phen)
NULL

##' Park Falls MODIS reflectance data
##'
##' @rdname PFa_refl
##' @name PFa_refl
##' @docType data
##' @format data frame containing MODIS reflectances for the Park Falls, Wisconsin tall tower Ameriflux site.  The data frame contains the variables:
##' \describe{
##' \item{date}{chron object; timestamps (date and time)}
##' \item{red}{the reflectance value for the red band}
##' \item{nir}{the reflectance value for the near infrared (NIR) band}
##' \item{blue}{the reflectance value for the blue band}
##' \item{swir}{the reflectance value for the shortwave infrade (SWIR) band}
##' }
##' @note PFa_refl is part of the Park_Falls dataset. See
##' documentation for 'Park_Falls' for further details.
##' @source These data are extracted from the Oak Ridge National Laboratory MODIS subsets for MODIS product MOD09A1.
##' @seealso \code{\link{Park_Falls}}
##' @references Oak Ridge National Laboratory Distributed Active Archive Center (ORNL DAAC). 2011. MODIS subsetted land products, Collection 5. Available on-line [http://daac.ornl.gov/MODIS/modis.html] from ORNL DAAC, Oak Ridge, Tennessee, U.S.A. Accessed 17 Dec, 2013. 
##' @examples
##' data(Park_Falls)
##' with(PFa_refl, plot(date, red,
##'                     col='red', type='b', pch='o',
##'                     xlab='date', ylab='reflectance',
##'                     ylim=range(PFa_refl[ , c('red', 'blue')],
##'                                na.rm=TRUE),
##'                     main='Park Falls MODIS reflectance'))
##' with(PFa_refl, points(date, blue,
##'                       col='blue', type='b', lty='dashed', pch='*'))
##' legend( x='topright',
##'         legend=c('red band', 'blue band'),
##'         col=c('red', 'blue'), lty=c('solid', 'dashed'), pch=c('o', '*'))
NULL

