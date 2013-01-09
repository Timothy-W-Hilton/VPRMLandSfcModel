\name{getPscale}
\alias{getPscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPscale(tow, phen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tow}{ ~~Describe \code{tow} here~~ }
  \item{phen}{ ~~Describe \code{phen} here~~ }
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
function (tow, phen) 
{
    Pscale <- (1 + tow$LSWI)/2
    mean_gmax <- mean(strptime(as.character(phen$gmax), format = "(\%m/\%d/\%y \%H:\%M:\%S)")$yday)
    mean_gdec <- mean(strptime(as.character(phen$gdec), format = "(\%m/\%d/\%y \%H:\%M:\%S)")$yday)
    for (y in unique(month.day.year(tow$date)$year)) {
        gmax <- phen[phen$year == y, "gmax"]
        gdec <- phen[phen$year == y, "gdec"]
        if (length(gmax) == 0) 
            gmax <- chron(paste(1, 1, y, sep = "/")) + mean_gmax
        if (length(gdec) == 0) 
            gdec <- chron(paste(1, 1, y, sep = "/")) + mean_gdec
        Pscale <- zoo(Pscale, order.by = tow$date)
        window(Pscale, start = gmax, end = gdec) <- 1
        Pscale <- as.vector(Pscale)
    }
    return(Pscale)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
