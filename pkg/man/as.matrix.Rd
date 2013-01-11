\name{as.matrix}
\alias{as.logical}
\alias{as.numeric}
\alias{as.character}
\alias{as.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Species Matrix with Specified Storage Mode
}
\description{
The functions retrieve a species matrix of mode according the methods name and \code{as.matrix} returns corresponding types of matrices but specified by its argument \code{mode}.
}
\usage{
as.logical(x)

as.numeric(x)

as.character(x)

as.matrix(x, mode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An object inheriting from class \code{VegsoupData}.
}
  \item{mode}{
A character string giving the desired mode or type of the object. Can be one of \code{"character"}, \code{"numeric"} or \code{"logical"}.
}

}
\details{
For any object inheriting from class \code{VegsoupData} the methods return an object of class \code{"matrix"} with dimnames giving plot and species names. The latter are strings where any occurrence in a layer is labeled using the layer name and the species abbreviations, collapased with the \code{"@"} gylph (see \code{\link{Taxonomy}}). The returned object is suitable for input of functions in other packages that require an object of class \code{matrix}. 

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{AbundanceScale(obj)\$lims}. Absences are zeros. Equals \code{as.matrix(obj, mode = "numeric")}.

\code{as.character} returns a matrix of mode \code{"character"} with species abundances in the data sets original scale \code{AbundanceScale(obj)\$codes}. Absences are coded as dots. Equals \code{as.matrix(obj, mode = "character")}.

\code{as.logical} returns a matrix of mode \code{"numeric"} with zeros coding for species absences and ones for presences. Equals \code{as.matrix(obj, mode = "logical")}.

Note, \code{as.logical} and \code{as.matrix(obj, "logical")} do not return a boolean or logical matrix composed of \code{TRUE} and \code{FALSE} but use ones to code presences (\code{TRUE}) and zeros (\code{FALSE}) for absences. The mode of the returned matrix is of type numeric.

}
\value{
A matrix of mode \code{"numeric"} or \code{"character"} (see \sQuote{details}).
}
\author{
Roland Kaiser
}
\note{
If the object has non \code{NULL} values in its data standardization slot the method defined by \code{decostand(obj)} will automatically be applied!
}
\seealso{
\code{\link{Vegsoup}}, \code{\link{VegsoupData}}, \code{\link{decostand}} and \code{\link{AbundanceScale}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))

#	presence/absence representation
dta.pa <- as.logical(dta)
#	the same results can be achieved using the classes decostand property
decostand(dta) <- "pa"

#	note, that the results of as.numeric() and as.logical()
#	are the same if presence/absence (transformation) is applied
dta.pa2 <- as.numeric(dta)

#	for a true boolean matrix change mode
mode(dta.pa) <- "logical"
head(dta.pa)
all.equal(dta.pa, dta.pa2, check.attributes = FALSE)

require(vegan)

pro <- procrustes(as.logical(dta), as.numeric(dta))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

