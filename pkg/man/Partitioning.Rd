\encoding{UTF-8}

\name{Partitioning}
\docType{methods}

\alias{getK}
\alias{getK,VegsoupPartition-method}

\alias{partitioning}
\alias{partitioning,VegsoupPartition-method}

\alias{partitioningMatrix}
\alias{partitioningMatrix,VegsoupPartition-method}

\alias{partitioningCombinations}
\alias{partitioningCombinations,VegsoupPartition-method}

\title{ Partitioning Vector of VegsoupPartition objects }

\description{
  Retrieve number of partitions (\code{getK}), partitioning vector (\code{partitioning}),
  create a matrix of memberships of plots to partitions (\code{partitioningMatrix})
  or tabulate all possibele combinations of partitions (\code{partitioningCombinations}).
}

\usage{
\S4method{getK}{VegsoupPartition}(x)

\S4method{partitioning}{VegsoupPartition}(x)

\S4method{partitioningMatrix}{VegsoupPartition}(x)

\S4method{partitioningCombinations}{VegsoupPartition}(x, collapse)
}

\arguments{
  \item{ x }{
	\code{'VegsoupPartition'} object.
  }
  \item{ collapse }{
	Character be be used as separator in dimnames. Only characters are
	allowed that can be identified with \code{grep("[:punct:]", collapse)}.
	See \code{\link{regex}} in package \pkg{base} for details.
  }
}

\value{
  \item{ partitioning }{
	returns the running partitioning vector.
  }
  \item{ partitioningMatrix }{
	returns a \code{matrix} of zeros and ones where rows are plots and
	columns are partitions. Ones indicate to which partition a plot belongs.
  }
  \item{ partitioningCombinations }{
	returns a \code{matrix} of combinations where column names indicate a particular
	combination.
  }
}

\author{ Roland Kaiser }

\seealso{
\code{\link{getK}}, \code{\linkS4class{VegsoupPartition}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

# number of partitions
getK(x)

# vector of assigments
partitioning(x)

# matrix of memberships
partitioningMatrix(x)

# matrix of possible combinations
partitioningCombinations(x)
}
\keyword{ methods }

