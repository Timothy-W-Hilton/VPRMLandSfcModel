# VPRMLandSfcModel-package: R implementation of VPRM with parameter estimation

# Description:

Provides an R implementation of the Vegetation Photosynthesis and
Respiration Model (VPRM) of Mahadevan et al (2008) and tools to
estimate VPRM parameter values from observations.  VPRM is a
simple diagnostic land surface model based on light-use
efficiency.  VPRM diagnoses gross ecosystem exchange (GEE) of
carbon dioxide, ecosystem respiration (R), and net ecosystem
exchange (NEE) of carbon dioxide.  VPRM parameters may be
estimated separately for different subsets of a dataset of
observations.  If the Rmpi package is installed, such separate
parameter estimations may be run in parallel.

# Details:

- Package:   VPRMLandSfcModel
- Type:      Package
- Version:   1.2.1
- Date:      2017-10-03
- License:   GPL-3
- LazyLoad:  yes


The literature in the references section provides a detailed
description of VPRM.

The top-level function for running VPRM is `vprm_calc_NEE`.
`vprm_calc_NEE` calculates net ecosystem exchange (NEE) according to
the VPRM (Mahadevan et al (2008) eqn 12).

The top-level function for estimating VPRM parameters from
observations is `estimate_VPRM_pars`.

The other functions provide methods to obtain the necessary driver
data and to calculate some of the intermediate quantities that
VPRM derives from remote sensing observations (e.g. land surface
water index, Tscale, Pscale, etc.).

The package includes two datasets:
- Park_Falls contains ground-observed as well as satellite-observed
     driver data for VPRM from the Park Falls, Wisconsin, USA
     Ameriflux site.
- VPRM_parameters contains values for VPRM parameters lambda, PAR_0,
     alpha, and beta.  The parameter values were determined by
     optimizing VPRM using 65 North American eddy covariance sites
     as described in Hilton etal (2013).

# Installation

```R
library('devtools')
install_github('Timothy-W-Hilton/VPRMLandSfcModel')
```

# Usage

The vignette document 'VPRM_vignette', included with the package,
provides several "getting started" usage examples.

```R
library('VPRMLandSfcModel')
RShowDoc('VPRM_vignette', package='VPRMLandSfcModel')
```

# Author(s):

Timothy W. Hilton

Maintainer: Timothy W. Hilton <t.hilton@gns.cri.nz>

# References:

To cite the Vegetation Photosynthesis Respiration Model (VPRM)
itself, use:
> Mahadevan, P., Wofsy, S.C., Matross, D.M., Xiao, X., Dunn, A.L.,
> Lin, J.C., Gerbig, C., Munger, J.W., Chow, V.Y., and Gottlieb,
> E.W., 2008. A satellite-based biosphere parameterization for net
> ecosystem CO2 exchange: Vegetation photosynthesis and respiration
> model (VPRM). Global Biogeochemical Cycles, 22, GB2005.
> doi:10.1029/2006GB002735.

To cite the R package 'VPRMLandSfcModel', use:
> Hilton, T. W., Davis, K. J., Keller, K., and Urban, N. M.:
> Improving North American terrestrial CO2 flux diagnosis using
> spatial structure in land surface model residuals, Biogeosciences,
> 10, 4607-4625, doi:10.5194/bg-10-4607-2013, 2013.

Additionally, the code itself now has a DOI: [![DOI](https://zenodo.org/badge/33335415.svg)](https://zenodo.org/badge/latestdoi/33335415)
