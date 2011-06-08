\name{commons}
\alias{commons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
commons(x, treshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{treshold}{
%%     ~~Describe \code{treshold} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, treshold) 
{
    if (!inherits(x, "LaTex.species")) 
        stop("Need object of class LaTex.species")
    if (missing(treshold)) 
        cat("\ntry to find treshold")
    spc <- slot(x, "query")$abbr
    occ <- aggregate(rep(1, length(spc)), by = list(spc), sum)
    names(occ) <- c("abbr", "occu")
    if (missing(treshold)) {
        treshold <- fivenum(occ$occu)[4]
        cat("\n use", treshold, "as abundance treshold")
    }
    tmp <- occ[occ$occu >= treshold, 1]
    res <- match(tmp, occ$abbr)
    res <- list(commons = res, treshold = treshold, occu = occ)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
