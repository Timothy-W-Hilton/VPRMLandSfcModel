##' flux-tower-optimized VPRM parameter values
##'
##' The dataset VPRM_parameters contains the parameter values for the all-sites--all-avaialable data and the PFT-all-available data parmameter estimations described in Hilton et al (2013) section 2.3.
##' @name VPRM_parameters
##' @docType data
##' @format The dataset VPRM_parameters contains four data frames:
##' \describe{
##' \item{all_all_VPRM_parameters}{all-sites--all-available-data parameters from Hilton et al (2013).}
##' \item{pft_all_VPRM_parameters}{PFT--all-time parameters from Hilton et al (2013).  "PFT" abbreviates "Plant Functional Type".}
##' }
##' @source The VPRM parameter values were determined by optimizing to
##' a set of 65 North American eddy covariance sites as described in
##' Hilton et al (2013).
##' @author Timothy W. Hilton
##' @seealso \code{\link{all_all_VPRM_parameters}},
##' \code{\link{pft_all_VPRM_parameters}},
##' @references Hilton, T. W., Davis, K. J., Keller, K., and Urban,
##' N. M.: Improving North American terrestrial CO2 flux diagnosis
##' using spatial structure in land surface model residuals,
##' Biogeosciences, 10, 4607-4625, doi:10.5194/bg-10-4607-2013, 2013.
##' @examples
##' data(VPRM_parameters)
##' print(pft_all_VPRM_parameters)
NULL

##' VPRM parameter set
##'
##' This is the all-sites--all-available-data parameter set described
##' in Hilton et al (2013) section 2.3.
##' @rdname all_all_VPRM_parameters
##' @name all_all_VPRM_parameters
##' @docType data
##' @format data frame containing values for the VPRM parameters
##' lambda, PAR_0, alpha, and beta.
##' @note all_all_VPRM_parameters is part of the
##' \code{\link{VPRM_parameters}} dataset
##' @source The parameter values were determined by optimizing to a
##' set of 65 North American eddy covariance sites as described in
##' Hilton et al (2013) section 2.3.
##' @author Timothy W. Hilton
##' @references Hilton, T. W., Davis, K. J., Keller, K., and Urban,
##' N. M.: Improving North American terrestrial CO2 flux diagnosis
##' using spatial structure in land surface model residuals,
##' Biogeosciences, 10, 4607-4625, doi:10.5194/bg-10-4607-2013, 2013.
##' @seealso \code{\link{VPRM_parameters}}
##' @examples
##' data(VPRM_parameters)
##' print(all_all_VPRM_parameters)
NULL

##' VPRM parameter set
##'
##' This is the PFT--all-available-data parameter set described in
##' Hilton et al (2013) section 2.3.  PFT abbreviates "Plant
##' Functional Type".  The PFTs are from the IGBP land cover
##' classification scheme (Loveland and Belward 1997).
##' @rdname pft_all_VPRM_parameters
##' @name pft_all_VPRM_parameters
##' @docType data
##' @format data frame containing values for the VPRM parameters lambda, PAR_0, alpha, and beta by plant functional type.  The PFT variable is a factor with levels:
##' \describe{
##' \item{W}{Water}
##' \item{ENF}{Evergreen Needleleaf forest}
##' \item{EBF}{Evergreen Broadleaf forest}
##' \item{DNF}{Deciduous Needleleaf forest}
##' \item{DBF}{Deciduous Broadleaf forest}
##' \item{MF}{Mixed forest}
##' \item{CS}{Closed shrublands}
##' \item{OS}{Open shrublands}
##' \item{WS}{Woody savannas}
##' \item{SAV}{Savannas}
##' \item{GRA}{Grasslands}
##' \item{WET}{Permanent wetlands}
##' \item{CRO}{Croplands}
##' \item{URB}{Urban and built-up}
##' \item{CNM}{Cropland/Natural vegetation mosaic}
##' \item{SNO}{Snow and ice}
##' \item{BAR}{Barren or sparsely vegetated}
##' }
##' @note pft_all_VPRM_parameters is part of the
##' \code{\link{VPRM_parameters}} dataset
##' @source The parameter values were determined by optimizing to a
##' set of 65 North American eddy covariance sites as described in
##' Hilton et al (2013) section 2.3
##' @author Timothy W. Hilton
##' @references Hilton, T. W., Davis, K. J., Keller, K., and Urban,
##' N. M.: Improving North American terrestrial CO2 flux diagnosis
##' using spatial structure in land surface model residuals,
##' Biogeosciences, 10, 4607-4625, doi:10.5194/bg-10-4607-2013, 2013.
##' @references Loveland, T. and Belward, A.: The IGBP-DIS global 1km
##' land cover data set, DISCover: first results, Int. J. Remote
##' Sens., 18, 3289-3295, DOI:10.1080/014311697217099, 1997.
##' @seealso \code{\link{VPRM_parameters}}
##' @examples
##' data(VPRM_parameters)
##' print(pft_all_VPRM_parameters)
NULL
