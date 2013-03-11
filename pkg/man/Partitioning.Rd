\name{Partitioning}
\docType{methods}
\alias{Partitioning}
\alias{Partitioning-methods}
\alias{PartitioningMatrix}
\alias{PartitioningMatrix-methods}
\alias{PartitioningMatrix,VegsoupPartition-method}
\alias{PartitioningCombinations}
\alias{PartitioningCombinations-methods}
\alias{PartitioningCombinations,VegsoupPartition-method}

\title{ Methods for the Partitioning Vector of \code{"VegsoupPartition"} Objects }
\description{
  Retreive partitioning vector, create a matrix of memberships of plots to
  partitions or tabulate all possibele combinations of partitions.
}
\usage{
## S4 method for signature 'VegsoupPartition'
Partitioning(obj)

## S4 method for signature 'VegsoupPartition'
PartitioningMatrix(obj)

## S4 method for signature 'VegsoupPartition'
PartitioningCombinations(obj, collapse)
}
\arguments{
  \item{ obj }{
    An object inheriting from class \code{"VegsoupPartition"}.
  }
  \item{ collapse }{
    Character of be be used as seperator in dimnames. Only characters are
    allowed that can be iddentified with \code{grep("[:punct:]", collapse)}.
    See \code{\link{regex}} in package base.
  }
}

\value{
  \item{Partitioning}{
    returns the running partitioning vector.
  }
  \item{PartitioningMatrix}{
    returns a \code{"matrix"} of zeros and ones where rows are plots and
    columns are partitons. Ones indicate to which partition a plot belongs.
  }
  \item{PartitioningCombinations}{
    returns a \code{"matrix"} of combinations coded within column names.
  }
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{getK}}
\code{\linkS4class{VegsoupPartition}}
}
\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa,
       coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, 2)
PartitioningMatrix(prt)
}
\keyword{ methods }

