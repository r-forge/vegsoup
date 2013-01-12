\name{typal}
\alias{typal}
\alias{typal-methods}
\alias{typal-VegsoupDataPartition-method}

\title{
Typical Samples of an Object of Class VegsoupData*
}
\description{
Identifies samples (plots) that typify clusters in a partition based on dissimilarity.
}
\usage{
typal(obj, k=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ obj }{
An object inheriting from class \code{VegsoupDataPartition}. 
}
  \item{ \dots }{
Arguments passed to \code{\link{as.dist}}.
}
}
\details{
Silhouette analysis returns \code{k} samples with the largest positive silhouette width for each partition. Partana analysis returns the \code{k} samples with the highest mean similarity to the partition. See \code{\link[optpart:typal]{typal}} in package \pkg{optpart} for further ditails.
}
\value{
Returns a \code{list} with two components
\item{ partana }{
A \code{data.frame} with the clusters as rows and typal samples as columns.
}
\item{ silhouette }{
A \code{data.frame} as above.
}

}
\author{
A wrapper for function \code{typal} by David W. Roberts in \pkg{optpart}.
}
\seealso{
\code{\link{as.dist}}
}
\examples{
library(vegsoup)
data(testdata)
        
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
prt <- VegsoupDataPartition(dta, k = 2)

typal(prt)
typal(prt, typeof = "logical")

typal(prt, typeof = "logical", mode = "R")	
}
\keyword{ methods }
\keyword{ cluster }% __ONLY ONE__ keyword per line
