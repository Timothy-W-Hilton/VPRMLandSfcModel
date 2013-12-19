\name{VPRMLandSfcModel-package}
\alias{VPRMLandSfcModel-package}
\alias{VPRMLandSfcModel}
\docType{package}
\title{
R implementation of VPRM
}
\description{
Provides an R implementation of the Vegetation Photosynthesis and
Respiration Model (VPRM) of Mahadevan et al (2008).  VPRM is a simple
diagnostic land surface model based on light-use efficiency.  VPRM
diagnoses gross ecosystem exchange (GEE) of carbon dioxide, ecosystem
respiration (R), and net ecosystem exchange (NEE) of carbon dioxide.
}
\details{
\tabular{ll}{
Package: \tab VPRMLandSfcModel\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-12-18\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
The literature in the references section provides a detailed description
of VPRM.

The top-level function is vprm_calc_NEE.  vprm_calc_NEE calculates net
ecosystem exchange (NEE) according to the VPRM (Mahadevan et al (2008)
eqn 12).

The other functions provide methods to obtain the necessary driver data
and to calculate some of the intermediate quantities that VPRM derives
from remote sensing observations (e.g. land surface water index, Tscale,
Pscale, etc.).
}
\author{
Timothy W. Hilton

Maintainer: Timothy W. Hilton <thilton@ucmerced.edu>
}
\references{
  To cite the Vegetation Photosynthesis Respiration Model
  (VPRM) itself, use:\cr
  Mahadevan, P., Wofsy, S.C., Matross, D.M., Xiao, X., Dunn, A.L., Lin,
  J.C., Gerbig, C., Munger, J.W., Chow, V.Y., and Gottlieb, E.W., 2008. A
  satellite-based biosphere parameterization for net ecosystem CO2
  exchange: Vegetation photosynthesis and respiration model (VPRM). Global
  Biogeochemical Cycles, 22, GB2005. doi:10.1029/2006GB002735.

  To cite the R package 'VPRMLandSfcModel', use:\cr
  Hilton, T. W., Davis, K. J., Keller, K., and Urban, N. M.: Improving
  North American terrestrial CO2 flux diagnosis using spatial structure
  in land surface model residuals, Biogeosciences, 10, 4607-4625,
  doi:10.5194/bg-10-4607-2013, 2013.
}
\keyword{ package }

