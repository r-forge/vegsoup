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
For any class containing \code{VegsoupData} the methods returns a \code{\dQuote{matrix}} with dimnames giving plot and species names. The latter are strings where any occurrence in a layer is labeled using the layer name and the species´s abbreviations, collapased with the \code{\dQuote{@}} gylph (see \code{\link{Taxonomy}}). The returned object is a input suitable for functions in other packages that require an object of class \code{matrix}.

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{AbundanceScale(obj)\$lims}.

\code{as.numeric} returns a matrix of mode \code{"character"} with species abundances in the data sets original scale \code{AbundanceScale(obj)\$codes}.

\code{as.binary} returns a matrix of mode \code{"numeric"} with zeros coding species presences.

If the object has non \code{NULL} values for data standardization (invoke \code{decostand(obj)} to get )
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

#	presence/absence representation
dta.pa <- as.binary(dta)
#	the same results can be achieved using the classes decostand property
decostand(dta) <- "pa"

#	note, that the results of as.numeric() and as.binary()
#	are the same if presence/absence (transformation) is applied
dta.pa2 <- as.numeric(dta)

all.equal(dta.pa, dta.pa2, check.attributes = FALSE)

require(vegan)

pro <- procrustes(as.binary(dta), as.numeric(dta))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

