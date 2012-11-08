\name{as.binary}
\alias{as.binary}
\alias{as.numeric}
\alias{as.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cast Species Matrix with Specified Storage Mode
}
\description{
The function creates a matrix of mode according the methods name.
}
\usage{
as.binary(obj)

as.numeric(obj)

as.character(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{VegsoupData}
}
}
\details{
For any class containing \code{VegsoupData} the methods returns a \code{\dQuote{matrix}} with dimnames giving plot and species names. The latter are strings where any occurrence in a layer is labeled using the layer name and the species´s abbreviations, collapased with the \code{\dQuote{@}} gylph (see \code{\link{Taxonomy}}). The returned object is a suitable input for functions in other packages.

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{AbundanceScale(obj)\$lims}.
\code{as.numeric} returns a matrix of mode \code{"character"} with species abundances in the data sets original scale \code{AbundanceScale(obj)\$codes}.
\code{as.binary} returns a matrix of mode \code{"numeric"} with zeros coding species presences.
}
\value{
A matrix of mode \code{"numeric"} or \code{"character"} (see \sQuote{details})
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{Vegsoup}}, \code{\link{VegsoupData}}, \code{\link{AbundanceScale}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
(as.binary(dta))

require(vegan)

pro <- procrustes(as.binary(dta1), as.numeric(dta1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

