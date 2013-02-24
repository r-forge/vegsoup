\name{richness}
\alias{richness}
\alias{richness-method}

\title{ Species Richness of an Object of Class Vegsoup* }
\description{
Retrieve dataset, sample or grouped species richness.
}
\usage{

## S4 method for class Vegsoup
richness(obj, choice = c("dataset", "sample"), ...)

## S4 method for class VegsoupPartition
richness(obj, choice = c("dataset", "sample", "partition"), ...)

}

\arguments{
  \item{obj}{
A \code{Vegsoup*} object.
}
  \item{choice}{
Return either dataset or sample richness, defaults to \code{"dataset"}. Partial matches are allowed. Objects of class \code{"VegsoupPartition"} accept  an additional option \code{choice = "partition"}. This returns .
}
  \item{\dots}{
Not implemented.
}
}
\details{
Dataset richness is defined as the number of species (discarding layer duplicates) found in the data set. Sample richness is the number of species per sample (plot) and partition richness is the pooled species richness per partition. In order to calculate correct species richness, in cases when the object has more than one layer, all occurrences of a particular species in different layers are collapsed to one.
}
\value{
A numeric vector of suitable size with names taken from the object, except \code{choice = "dataset"}, where names are meaningless
}
\author{
Roland Kaiser
}
\seealso{
\code{\link[vegsoup:rowSums]{rowSums}},\code{\link[vegsoup:colSums]{colSums}}
}
\examples{
library(vegsoup)
data(testdata)        
dta <- VegsoupDataVegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
	
richness(dta) # default choice = "dataset"
richness(dta, "sa") # partial match

# rowSums counts each occurence an layers.
# Because there are 3 layers the numbers are different
# compared to richness(dta, "sample")!
rowSums(dta)
}
\keyword{ methods }

