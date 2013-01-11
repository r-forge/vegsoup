\name{row+colnames}
\docType{methods}
\alias{rownames}
\alias{rownames-methods}
\alias{colnames}
\alias{colnames-methods}
\alias{dimnames}
\alias{dimnames-methods}
\alias{names}
\alias{names-methods}

\title{Row and Column Names of a VegsoupData* object}
\description{
Retrieve the row or column names of the species \code{matrix} and the names of the sites \code{data.frame}.
}
\usage{
rownames(obj)
colnames(obj)
dimnames(obj)
names(obj)	 

names(obj) <- value	 
}
\arguments{
  \item{obj}{
an object from class \code{VegsoupData*}. 
}
  \item{value}{
a character vector of the same length as \code{names(obj)}, or NULL.
}
}
\details{
Function \code{dimnames} returns \code{dimnames(as.matrix(obj))}. Correspondingly, \code{colnames} equals to \code{dimnames(as.matrix(obj))[[2]]}. This applies only to the species matrix which is of class \code{matrix}. Note, \code{names} when called on an object of class \code{matrix} will return \code{NULL} in base R. Therefore the \code{names} method is only defined for the sites slot and equals \code{names(Sites(obj))}). As both the species matrix and the sites data have the same number of rows (plots) in exactly the same order, the \code{rownames} method will return the same for both data partitions.
}
\author{
Roladn Kaiser
}
\examples{
library(vegsoup)
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
colnames(dta)
rownames(dta)
dimnames(dta)

names(dta)
}
\keyword{methods}

