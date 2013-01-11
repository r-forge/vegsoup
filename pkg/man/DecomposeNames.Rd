\name{DecomposeNames}
\alias{DecomposeNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deparse Taxa names for Objects of Class VegsoupData*
}
\description{
Parses the \code{colnames} of the species matrix of an object into four components.
}
\usage{
DecomposeNames(obj, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An object inheriting from class \code{VegsoupData}.
}
  \item{verbose}{
Additional output.
}

}
\details{
The function parses the \code{colnames} of the species matrix into four components. The first giving the compound taxon key (\code{"abbr.layer"}), the second the taxon abbreviation only (\code{"abbr"}), the third the layer code (\code{"layer"}) and the last component corresponds to the full taxon name \code{"taxon"} queryied from \code{Taxonomy(obj)}. \code{\link{VegsoupData}} constructs these \dQuote{compound taxon abbreviations} by pasting \code{make.names(x)} and \code{layer} seperated with \code{"@"}. To deparse the abbreviation and layer string \code{DecomposeNames} should be used.
}
\value{
A data.frame with as many rows and in the same order as there are columns in the species matrix. Note, the method uses \code{invisible} when returning it's results.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{VegsoupData}},\code{\link{Taxonomy}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
txa <- DecomposeNames(dta)
head(txa)
all.equal(txa$abbr.layer, colnames(dta))		
}

\keyword{ methods }

