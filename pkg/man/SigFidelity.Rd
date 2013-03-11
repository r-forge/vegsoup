\name{SigFidelity}
\docType{methods}
\alias{SigFidelity}
\alias{SigFidelity-methods}
\alias{SigFidelity,VegsoupPartition-method}

\title{ Experimental Function }
\description{
Experimental Function
}
\usage{
SigFidelity(obj, ...)
}
\arguments{
  \item{obj}{
    A \code{"VegsoupPartition"} object.
}
  \item{ mode }{
    Type of analysis. See \sQuote{Details}.
  }
  \item{ \dots }{
    Arguments passeed to \dots.
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
