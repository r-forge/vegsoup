\encoding{UTF-8}

\name{richness}
\docType{methods}

\alias{richness}
\alias{richness,Vegsoup-method}
\alias{richness,VegsoupPartition-method}

\title{ Species Richness }

\description{
Retrieve dataset, sample or grouped species richness.
}

\usage{
\S4method{richness}{Vegsoup}(obj, choice = c("dataset", "sample"))
\S4method{richness}{VegsoupPartition}(obj, choice = c("dataset", "sample", "partition"))
}

\arguments{
  \item{ obj }{
    \code{Vegsoup*} object.
  }
  \item{ choice }{
    Return either dataset or sample richness, defaults to \code{dataset}.
    Partial matches are allowed. Objects of class \code{'VegsoupPartition'}
    take an additional option \code{"partition"}.
  }
}

\details{
Dataset richness is defined as the number of species (discarding layer replicates) found in the data set. Sample richness is the number of species per sample (plot) and partition richness is the pooled species richness per partition. In order to calculate correct species richness, in cases when the object has more than one layer, all occurrences of a particular species in different layers are collapsed to one.
}

\value{
A numeric vector of suitable size with names taken from the object, except \code{choice = "dataset"}, where names are meaningless
}
\author{
Roland Kaiser
}
\seealso{
\code{\link[vegsoup]{rowSums}},\code{\link[vegsoup]{colSums}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

richness(x) # default choice = "dataset"
richness(x, "sa") # partial match

# rowSums counts each species-layer replication (pseudo-species).
# Because there are 3 layers the numbers are different
# compared to richness(dta, "sample")!
rowSums(x)

# extact number of unique species per partition
richness(VegsoupPartition(x, k = 2), "partition")
}
\keyword{ methods }

