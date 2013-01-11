\name{richness}
\alias{richness}
\alias{richness-method}

\title{ Dataset or Sample Richness of an Object of Class VegsoupData* }
\description{
Retrieve dataset or sample richness.
}
\usage{
richness(obj, choice = c("dataset", "sample"), ...)
}

\arguments{
  \item{obj}{
an object of class \code{VegsoupData*}.
}
  \item{choice}{
Return either dataset or sample richness, defaults to \code{"dataset"}. Partial matches are allowed.
}
  \item{\dots}{
Not implemented.
}
}
\details{
Dataset richness is defined as the number of species (discarding layer duplicates) found in the data set. Sample richness is the number of species per sample (plot). In order to calculate correct species richness, when the object has more than one layer, all occurences of a particular species in different layers are collapsed to one.
}
\value{
A numeric vector of suitable size with names taken from the object if \code{choice = "sample"}.
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
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
	
richness(dta) # default choice = "dataset"
richness(dta, "sa") # partial match

# rowSums counts each occurence an layers.
# Because there are 3 layers the numbers are different
# compared to richness(dta, "sample")!
rowSums(dta)
}
\keyword{ methods }

