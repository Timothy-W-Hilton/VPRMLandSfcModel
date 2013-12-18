\name{VPRM-package}
\alias{VPRM-package}
\alias{VPRM}
\docType{package}
\title{
R implementation of VPRM
}
\description{
Provides an R implementation of the Vegetation Photosynthesis and
Respiration Model (VPRM).  VPRM is a simple light-use efficiency based
diagnostic land surface model.  VPRM diagnoses gross ecosystem exchange
(GEE) of carbon dioxide, ecosystem respiration (R), and net ecosystem
exchange (NEE) of carbon dioxide.
}
\details{
\tabular{ll}{
Package: \tab VPRM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-08-13\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
The literature in the references section provides a detailed description
of VPRM.

The top-level function is NEE.  NEE calculates net ecosystem exchange
(NEE) according to the VPRM model.

The other functions provide methods to calculate some of the
intermediate quantities that VPRM derives from remote sensing
observations (e.g. land surface water index, Tscale, Pscale, etc.).
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

  To cite the R package 'VPRM', use:\cr
  Hilton, T. W., Davis, K. J., Keller, K., and Urban, N. M.: Improving
  North American terrestrial CO2 flux diagnosis using spatial structure
  in land surface model residuals, Biogeosciences, 10, 4607-4625,
  doi:10.5194/bg-10-4607-2013, 2013.
}
\keyword{ package }

