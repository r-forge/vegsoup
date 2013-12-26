\name{SigFidelity}
\docType{methods}

\alias{SigFidelity}
\alias{SigFidelity,VegsoupPartition-method}

\title{ Experimental Function }

\description{
Experimental.
}

\usage{
\S4method{SigFidelity}{VegsoupPartition}(obj, mode = 1, nperm = 999, alternative = "two.sided", verbose = TRUE)
}

\arguments{
  \item{obj}{
    \code{'VegsoupPartition'} object.
}
  \item{ mode }{
    type of analysis. See \sQuote{Details}.
  }
  \item{ nperm }{
    number of permutations
  }
  \item{ alternative }{
    alternative hypthersis.
  }
  \item{ verbose }{
    more output.
  }
}

\details{
\code{mode = 0} looks for species whose abundance is significantly higher in one of the groups. \code{mode = 0} looks for species whose abundance is significantly higher in sites belonging to one group as opposed to sites not belonging to it.
}

\value{
  An object of class \code{"VegsoupPartitionFidelity"}
}

\author{
Adapted from function \code{signassoc()} of package \pkg{indicspecies} by Miquel De Cáceres Ainsa.
}

\keyword{ methods }
