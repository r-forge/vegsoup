\name{Extract by Index}
\docType{methods}
\alias{Extract}

\alias{[}
\alias{[,Vegsoup-method}

\alias{Partition}
\alias{Partition,Vegsoup-method}

\title{ Indexing to Extract Values of a Vegsoup* Object }

\description{
  Extract parts of an object. Currently, a replace method is not implemented!
}

%\S4method{[}{Vegsoup}(x, i, j, ... , drop = TRUE)
\usage{
x[i, j, ... , drop = TRUE]

\S4method{Partition}{Vegsoup}(x, value)
}

\arguments{
  \item{ x }{
    A \code{Vegsoup*} object or for \code{Partition} a \code{VegsoupPartition} object.
  }
   \item{ i, j, \dots }{
    Elements to extract or replace.
  }
  \item{ drop }{
    Not used. The method will allways return a valid \code{Vegsoup*} object!
  }
  \item{ value }{
    An \code{integer} defining a partiton to be subseted.
  }

}

\details{
  If only one plot is requested for indexing (e.g. \code{x[1,]}) the \code{summary} and
  \code{show} methods additionally print a species list.
}

\note{
  When objects of class \code{VegsoupPartition} are subsetted it is likely that the
  partitioning vector (\code{Partitioning(obj)}) has to be subsetted too. If this is the case,
  the number of partitions \code{k} (\code{getK(obj)}) is changed (lowered) accordingly.
}

\author{ Roland Kaiser }

\seealso{
  \code{\link[vegsoup:subset]{subset}}
}

\examples{
require(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# assigment using object slots
Sites(dta) <- dta@sites[sample(nrow(dta)), ]

# note, in this case summary returns a species list
dta[1, ]
}
