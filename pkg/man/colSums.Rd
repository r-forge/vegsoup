\name{colSums}
\docType{methods}
\alias{colSums}
\alias{colSums-methods}
\alias{rowSums}
\alias{rowSums-methods}
\alias{rowSums}
\alias{rowSums-methods}
\alias{rowMeans}
\alias{rowMeans-methods}
\alias{colMeans}
\alias{colMeans-methods}

\title{ Row and Column Sums for Objects of Class VegsoupData* }
\description{
Form row and column sums for the the species matrix based on presence/absence and numeric matrices.
}
\usage{
colSums(x, typeof = "logical", ...)
rowSums(x, typeof = "logical", ...)

colMeans(x, na.rm = FALSE, dims = 1, typeof = "numeric", ...)
rowMeans(x, na.rm = FALSE, dims = 1, typeof  = "numeric", ...)
}
\arguments{
  \item{ x }{
An object of class VegsoupData*.
}
  \item{ typeof }{
The mode of the species matrix to compute values from, defaults to \code{"logical"} for \code{rowSums} and \code{colSums}. See \code{\link{as.matrix}} for details about argument \code{"typeof"}. The \code{rowMeans} and \code{colMeans} methods use \code{typeof = "numeric"} as the default and \code{typeof = "character"} is not supported by neither of the functions. 
}
  \item{\dots}{
Arguments passed to \code{\link[base:rowSums]{rowSums}}, \code{\link[base:rowSums]{rowSums}}, \code{\link[base:colSums]{colSums}} and \code{\link[base:colMeans]{colMeans}}, respectively.
}
}
\details{
Note, the default behaviour (argument \code{typeof} is missing) operates on a presence/absence version of the species matrix as can be obtained by \code{as.logical(x)} or \code{as.matrix(x, typeof = "logical")}. On this basis \code{colSums} returns the number of occurences of a species in the data set. The \code{rowSums} method returns the number of species with duplicated occurences of species on different layers if there is more than one layer and at least a single species occuring in more than one layer! In this case correct species richness can be obtained with method \code{richness(obj, "sample")}.

The \code{colSums}
}
\value{
A numeric vector of suitable size with names taken from the object.
}
\seealso{
\code{\link[base:colSums]{colSums}} in package \pkg{base}, \code{\link{as.matrix}} in package \pkg{vegsoup}, \code{\link{richness}}
}
\examples{
library(vegsoup)
data(testdata)        
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))

#	number of species including layer duplicates	
rowSums(dta)
#	sums of abundances 
rowSums(dta, typeof = "numeric")
#	Layers(dta) tells that there is more than one layers
#	some species occur in two layers
Layers(dta)
#	true species richness
richness(dta, "sample")

#	number of occurences for each species
colSums(dta)
#	sums of abundances
colSums(dta, typeof = "numeric")
}
\author{ Roladn Kaiser }

\keyword{methods}

