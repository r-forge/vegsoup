\name{DecomposeNames}
\docType{methods}
\alias{DecomposeNames}
\alias{DecomposeNames-methods}
\alias{DecomposeNames,VegsoupData-method}

\title{
Deparse Taxa names of Objects of Class VegsoupData*
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
The function parses the \code{colnames} of the species matrix into three components. The first giving the taxon abbreviation only (\code{"abbr"}), the second the layer code (\code{"layer"}) and the last component corresponds to the full taxon name \code{"taxon"} queryied from \code{Taxonomy(obj)}.

The compound taxon key (\code{"abbr.layer"}) can be accessed from the \code{rownames} of the resulting object. This is advantegous as it provides an easy means of subsetting with \code{colnames(obj)}.

\code{\link{VegsoupData}} constructs these \dQuote{compound taxon abbreviations} by pasting \code{make.names(x)} and \code{layer} seperated with \code{"@"}. To deparse the abbreviation and layer string \code{DecomposeNames} should be used.
}
\value{
A \code{"data.frame"} with as many rows and in the same order as there are columns in the species matrix. Note, the method uses \code{invisible} when returning it's results.
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

#	get scientific taxa names
m <- as.matrix(dta, typeof = "character", mode = "R")
txa[rownames(m), ]			
}

\keyword{ methods }

