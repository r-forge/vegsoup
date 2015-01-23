\encoding{UTF-8}

\name{Extract by Index}
\docType{methods}
\alias{Extract}

\alias{[}
\alias{[,Vegsoup-method}
\alias{[,Vegsoup,ANY,ANY,missing-method}
\alias{[,VegsoupPartition,ANY,ANY,missing-method}

\alias{Partition}
\alias{Partition,Vegsoup-method}
\alias{Partition,VegsoupPartition-method}
\alias{[,Partition,ANY,ANY,missing-method}

\title{ Indexing to Extract Values of a Vegsoup* Object }

\description{
  Extract parts of an object. Currently, a replace method is not implemented!
}

\usage{
\S4method{[}{Vegsoup}(x, i, j, ... , drop = TRUE)
\S4method{Partition}{Vegsoup}(x, value)
}

\arguments{
  \item{ x }{
    \code{Vegsoup*} object, for \code{Partition} a \code{VegsoupPartition} object.
  }
   \item{ i, j, \dots }{
    Elements to extract or replace.
  }
  \item{ drop }{
    Not used. The method will always return a valid \code{Vegsoup*} object!
  }
  \item{ value }{
    integer. An \code{integer} defining a partition to be subsetted. Can also be a vector.
  }

}

\details{
  If only one plot is requested for indexing (e.g. \code{x[1,]}) the \code{summary} and
  \code{show} methods additionally print a species list.
  
  Any doubled numerical indices are silently ignored.
}

\note{
  When objects of class \code{VegsoupPartition} are subsetted it is likely that the
  partitioning vector (\code{Partitioning(obj)}) has to be subsetted too. If this is the case,
  the number of partitions \code{k} (\code{getK(obj)}) is changed (lowered) accordingly.
}

\author{ Roland Kaiser }

\seealso{
  \code{\link[vegsoup]{subset}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# index the species matrix and get new objects
# select rows, the plots
x[ 1:2, ]
# select columns (the species) including layer replicates
x[ , sample(ncol(x), size = 10) ]
# note, sample may result in an object that holds less then 'size' species
# because of layer replication

# remove species with equal or less than three occurences,
# exact if there is no layer replication
any(colSums(x[, colSums(x) >= 3]) < 3)

# assigment using object slots
rownames(x)
sites(x) <- x@sites[sample(nrow(x)), ]
rownames(x)
}

\keyword{ methods }

