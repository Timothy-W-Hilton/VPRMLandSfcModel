\docType{data}
\name{VPRM_parameters}
\alias{VPRM_parameters}
\title{flux-tower-optimized VPRM parameter values}
\format{The dataset VPRM_parameters contains four data frames:
\describe{
\item{all_all_VPRM_parameters}{all-sites--all-available-data parameters from Hilton et al (2013).}
\item{pft_all_VPRM_parameters}{PFT--all-time parameters from Hilton et al (2013).  "PFT" abbreviates "Plant Functional Type".}
}}
\source{
  The VPRM parameter values were determined by optimizing
  to a set of 65 North American eddy covariance sites as
  described in Hilton et al (2013).
}
\description{
  The dataset VPRM_parameters contains the parameter values
  for the all-sites--all-avaialable data and the
  PFT-all-available data parmameter estimations described
  in Hilton et al (2013) section 2.3.
}
\examples{
data(VPRM_parameters)
print(pft_all_VPRM_parameters)
}
\author{
  Timothy W. Hilton
}
\references{
  Hilton, T. W., Davis, K. J., Keller, K., and Urban, N.
  M.: Improving North American terrestrial CO2 flux
  diagnosis using spatial structure in land surface model
  residuals, Biogeosciences, 10, 4607-4625,
  doi:10.5194/bg-10-4607-2013, 2013.
}
\seealso{
  \code{\link{all_all_VPRM_parameters}},
  \code{\link{pft_all_VPRM_parameters}},
}
\keyword{datasets}

