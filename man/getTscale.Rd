\name{getTscale}
\alias{getTscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTscale(T, Tmax, Tmin, Topt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{ ~~Describe \code{T} here~~ }
  \item{Tmax}{ ~~Describe \code{Tmax} here~~ }
  \item{Tmin}{ ~~Describe \code{Tmin} here~~ }
  \item{Topt}{ ~~Describe \code{Topt} here~~ }
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
function (T, Tmax, Tmin, Topt) 
{
    numer <- (T - Tmin) * (T - Tmax)
    denom <- ((T - Tmin) * (T - Tmax)) - (T - Topt)^2
    Tscale = numer/denom
    Tscale[which(Tscale < Tmin)] <- Tmin
    return(Tscale)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
