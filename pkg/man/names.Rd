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
\alias{$-methods}

\title{ Row and Column Names of a VegsoupData* object }
\description{
Retrieve the row or column names of the species \code{matrix} and the names of the sites \code{data.frame}.
}
\usage{
## S4 method for signature 'Vegsoup*'
rownames(obj)
## S4 method for signature 'Vegsoup*'
colnames(obj)
## S4 method for signature 'Vegsoup*'
dimnames(obj)
## S4 method for signature 'Vegsoup*'
names(obj)
## S4 replacement method for 'Vegsoup*' objects
names(obj) <- value
## S4 method for signature 'Vegsoup*'
x$name
## S4 replacement method for signature 'Vegsoup*'
x$i <– value
}
\arguments{
  \item{obj}{
An object from class \code{VegsoupData*}. 
}
  \item{value}{
A character vector of the same length as \code{names(obj)}, or NULL.
}
}
\details{
Function \code{dimnames} returns \code{dimnames(as.matrix(obj))}. Correspondingly, \code{colnames} equals to \code{dimnames(as.matrix(obj))[[2]]}. This applies only to the species matrix which is of class \code{matrix}. Note, \code{names} when called on an object of class \code{matrix} will return \code{NULL} in base R. Therefore the \code{names} method is only defined for the sites slot and equals \code{names(Sites(obj))}). As both the species matrix and the sites data have the same number of rows (plots) in exactly the same order, the \code{rownames} method will return the same for both data partitions.

The \code{"$"} operator can be used to access columns of the sites slot which is of class \code{data.frame} and has also functions to get or set the names of the object. Indexing of the species matrix can be done using indices specifying elements to extract. See \sQuote{Examples}.

}
\author{
Roland Kaiser
}
\seealso{
\code{\link{DecompseName}
}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# access dimnames of species matrix (class 'matrix')
colnames(dta) # pseudo species
rownames(dta) # plot names
dimnames(dta)

# return names of sites data (class 'data.frame')
names(dta)

# access names of sites data
dta$rock

# set a name
names(dta)[1] <- "bar"

# assign a value
if (any(names(dta) == "rock")) {
   dta$rock <- sqrt(dta$rock)
}
}
\keyword{ methods }
