\name{NEE_sin}
\alias{NEE_sin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NEE_sin(tower, lambda = NULL, alpha = NULL, beta = NULL, PAR_0 = NULL, a = NULL, b = NULL, c = NULL, d = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tower}{ ~~Describe \code{tower} here~~ }
  \item{lambda}{ ~~Describe \code{lambda} here~~ }
  \item{alpha}{ ~~Describe \code{alpha} here~~ }
  \item{beta}{ ~~Describe \code{beta} here~~ }
  \item{PAR_0}{ ~~Describe \code{PAR_0} here~~ }
  \item{a}{ ~~Describe \code{a} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{c}{ ~~Describe \code{c} here~~ }
  \item{d}{ ~~Describe \code{d} here~~ }
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
function (tower, lambda = NULL, alpha = NULL, beta = NULL, PAR_0 = NULL, 
    a = NULL, b = NULL, c = NULL, d = NULL) 
{
    if (is.null(lambda)) 
        lambda <- tower$lambda
    if (is.null(alpha)) 
        alpha <- tower$alpha
    if (is.null(beta)) 
        beta <- tower$beta
    if (is.null(PAR_0)) 
        PAR_0 <- tower$PAR_0
    doy <- as.integer(format(as.POSIXlt(tower$date), format = "\%j"))
    hr <- hours(tower$date)
    GEE <- (-1) * lambda * tower[, "Tscale"] * tower[, "Pscale"] * 
        tower[, "Wscale"] * (1/(1 + (tower[, "PAR"]/PAR_0))) * 
        tower[, "EVI"] * tower[, "PAR"]
    R <- alpha * tower[, "Tresp"] + beta
    yr.cycle <- a * sin((doy - b) * pi/365)
    day.cycle <- c * sin((hr - d) * pi/24)
    NEE <- GEE + R + yr.cycle + day.cycle
    return(NEE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
