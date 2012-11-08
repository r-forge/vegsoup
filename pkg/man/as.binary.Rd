\name{as.binary}
\alias{as.binary}
\alias{as.numeric}
\alias{as.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cast Species Matrix with Specified Storage Mode
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.binary(obj)
as.numeric(obj)
as.characteer(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{VegsoupData}
}
}
\details{
The function creates a matrix of mode according the methods name. \code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{AbundanceScale(obj)\$lims}
\code{as.numeric} returns a matrix of mode \code{"character"} with species abundances in the data sets original scale \code{AbundanceScale(obj)\$codes}
\code{as.binary} returns a matrix of mode \code{"numeric"} with zeros coding species presences.
}
\value{
a matrix of mode \code{"numeric"} or \code{"character"}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Vegsoup}}, \code{\link{VegsoupData}}, \code{\link{AbundanceScale}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
