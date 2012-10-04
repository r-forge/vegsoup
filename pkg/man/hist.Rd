\name{hist}
\alias{hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot histogram of fidelity values
}
\description{
Plots a histogram of fidelity values
}
\usage{
hist(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of \code{class(VegsoupDataPartitionFidelity)}
}
  \item{\dots}{
  arguments passed to \code{\link{hist}}
}
}
\details{
  See to \code{\link{hist}} for details.
}
\value{
an object of class \code{"histogram"}.
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
structure(function (x, ...) 
standardGeneric("hist"), generic = structure("hist", package = "graphics"), package = "graphics", group = list(), valueClass = character(0), signature = "x", default = structure(function (x, 
    ...) 
UseMethod("hist"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), generic = structure("hist", package = "graphics"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = structure(function (x, 
    ...) 
UseMethod("hist"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), generic = structure("hist", package = "graphics"), class = structure("derivedDefaultMethod", package = "methods"))(x, 
    ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
