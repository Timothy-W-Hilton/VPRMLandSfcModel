\name{getEVI}
\alias{getEVI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEVI(rho_nir, rho_red, rho_blue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho_nir}{ ~~Describe \code{rho_nir} here~~ }
  \item{rho_red}{ ~~Describe \code{rho_red} here~~ }
  \item{rho_blue}{ ~~Describe \code{rho_blue} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rho_nir, rho_red, rho_blue) 
{
    G <- 2.5
    C1 <- 6
    C2 <- 7.5
    L <- 1
    numer <- G * (rho_nir - rho_red)
    denom <- rho_nir + L + ((C1 * rho_red) - (C2 * rho_blue))
    EVI <- numer/denom
    return(EVI)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
