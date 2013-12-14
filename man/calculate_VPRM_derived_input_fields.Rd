\name{calculate_VPRM_derived_input_fields}
\alias{calculate_VPRM_derived_input_fields}
\title{calculate derived VPRM inputs from observations}
\usage{
  calculate_VPRM_derived_input_fields(obj)
}
\arguments{
  \item{obj}{VPRM_driver_data object; provides the
  observations necessary.}
}
\value{
  VPRM_driver_data object; same as input argument obj, but
  with derived fields filled in.
}
\description{
  calculate LSWI, Tscale, Pscale, Wscale, and Tresp from
  observed quantities.
}
\details{
  This is a helper function for VPRM_driver_data, and is
  not public.
}
\author{
  Timothy W. Hilton
}

