\name{Extract}
%% was $-methods
\docType{methods}
\alias{Extract}
\alias{[}
\alias{[-methods}
\alias{Partition}
\alias{Partition-methods}

\title{ Extract or Replace Parts Vegsoup* Objects }
\description{
Extract or replace parts of an object.
}

\usage{
## S4 method for signature 'Vegsoup*'
x[i, j, ... , drop = TRUE]

## S4 method for signature 'VegsoupPartition'
Partition(obj, value)
}
\arguments{
  \item{ x }{
    An object ingeriting from class \code{Vegsoup}.
  }
   \item{ i, j, \dots }{
    Elements to extract or replace.
  }
  \item{ drop }{
    Not used. The method will allways return a valid \code{Vegsoup*} object!
  }
  \item{ obj }{
    A \code{VegsoupPartition} object.
  }
  \item{ value }{
    An object of class \code{"integer"} defining a partiton to subset.
  }

}
\details{
If only one plot is requested for indexing (e.g. \code{obj[1,]}) the \code{summary} and \code{show} methods additionally will print a species list.
}
\note{
When objects of class \code{"VegsoupPartition"} are subsetted it is likely that the partitioning vector (\code{Partitioning(obj)}) has to be subsetted too. If this is the case, the number of partitions \code{"k"} (\code{getK(obj)}) is changed (lowered) accordingly.
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
