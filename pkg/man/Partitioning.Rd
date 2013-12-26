\name{Partitioning}
\docType{methods}

\alias{getK}
\alias{getK,VegsoupPartition-method}

\alias{Partitioning}
\alias{Partitioning,VegsoupPartition-method}

\alias{PartitioningMatrix}
\alias{PartitioningMatrix,VegsoupPartition-method}

\alias{PartitioningCombinations}
\alias{PartitioningCombinations,VegsoupPartition-method}

\title{ Partitioning vector of \code{VegsoupPartition} objects }

\description{
  Retrieve number of partitions (\code{getK}), partitioning vector (\code{Partitioning}),
  create a matrix of memberships of plots to partitions (\code{PartitioningMatrix})
  or tabulate all possibele combinations of partitions (\code{PartitioningCombinations}).
}

\usage{
\S4method{getK}{VegsoupPartition}(x)
\S4method{Partitioning}{VegsoupPartition}(x)
\S4method{PartitioningMatrix}{VegsoupPartition}(x)
\S4method{PartitioningCombinations}{VegsoupPartition}(x, collapse)
}

\arguments{
  \item{ x }{
    A \code{VegsoupPartition} object.
  }
  \item{ collapse }{
    Character be be used as seperator in dimnames. Only characters are
    allowed that can be identified with \code{grep("[:punct:]", collapse)}.
    See \code{\link{regex}} in package \pkg{base} for details.
  }
}

\value{
  \item{ Partitioning }{
    returns the running partitioning vector.
  }
  \item{ PartitioningMatrix }{
    returns a \code{matrix} of zeros and ones where rows are plots and
    columns are partitons. Ones indicate to which partition a plot belongs.
  }
  \item{ PartitioningCombinations }{
    returns a \code{matrix} of combinations where column names indicate a particular
    combination.
  }
}

\author{ Roland Kaiser }

\seealso{
\code{\link{getK}}, \code{\linkS4class{VegsoupPartition}}
}

\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa,
       coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, 2)

# number of partitions
getK(prt)
# vector of assigments
Partitioning(prt)
# matrix of meberships
PartitioningMatrix(prt)
# matrix of possible combinations
PartitioningCombinations(prt)
}
\keyword{ methods }

