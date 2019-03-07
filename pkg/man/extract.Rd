\encoding{UTF-8}

\name{Extract by Index}
\docType{methods}
\alias{Extract}

\alias{[}
\alias{[,Vegsoup-method}
\alias{[,Vegsoup,ANY,ANY,missing-method}
\alias{[,VegsoupPartition,ANY,ANY,missing-method}

\alias{partition}
\alias{partition,VegsoupPartition-method}
\alias{[,partition,ANY,ANY,missing-method}

\title{ Indexing to Extract Values of a Vegsoup* Object }

\description{
  Extract parts of an object. Currently, a replace method is not implemented!
}

\usage{
\S4method{[}{Vegsoup}(x, i, j, ... , drop = TRUE)
\S4method{partition}{VegsoupPartition}(x, value)
}

\arguments{
  \item{ x }{
    \code{Vegsoup*} object, for \code{partition} a \code{VegsoupPartition} object.
  }
   \item{ i, j, \dots }{
    Elements to extract or replace.
  }
  \item{ drop }{
    Not used. The method will always return a valid \code{Vegsoup*} object!
  }
  \item{ value }{
    \code{integer}. An \code{integer} defining a partition to be subsetted.
    Can also be a vector.
  }

}

\details{
  If only one plot is requested for indexing (e.g. \code{x[1,]}) the
  \code{summary} and \code{show} methods additionally print a species list.
  
  Any doubled numerical indices are silently ignored.
}

\note{
  When objects of class \code{VegsoupPartition} are subsetted it is likely that
  the partitioning vector (\code{partitioning(obj)}) has to be subsetted too.
  If this is the case, the number of partitions \code{k} (\code{getK(obj)}) is
  changed (lowered) accordingly.
}

\author{ Roland Kaiser }

\seealso{
\code{\link[vegsoup]{subset}},
\code{\link[vegsoup]{reorder}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# index the species matrix and get new objects
# select rows, the plots
# first two plots
x[ 1:2, ]

# select columns (the species) including layer replicates
# 10 random species
x[ , sample(ncol(x), size = 10) ]
# note, sample may result in an object that holds less then 'size' species
# because of layer replication

# remove species with equal or less than three occurences,
# exact if there is no layer replication
x[, colSums(x) >= 3]

any(colSums(x[, colSums(x) >= 3]) < 3)

# assigment using object slots
rownames(x)
sites(x) <- sites(x)[ sample(nrow(x)), ]
rownames(x)

# extract partitions
x <- VegsoupPartition(x, k = 3)
# extract partition 1
partition(x, 1)
# drop partition 2 and 3
partition(x, -c(2,3))
# extract partition 2 and 3
partition(x, c(2,3))
# as above using logical index vector
partition(x, c(FALSE, TRUE, TRUE))


}

\keyword{ methods }

