\name{quality}
\docType{methods}
\alias{Disdiam}
\alias{Disdiam-methods}
\alias{Silhouette}
\alias{Silhouette-methods}
\alias{Silhouette,VegsoupDataPartition-method}

\title{ Dissimilarity Diameters of a Partition }

\description{
  Calculate the diameter (maximum within-cluster dissimilarity) of all
  partitions, as well as the average diameter across all partitions using
  function \code{\link[optpart:disdiam]{disdiam}} of package optpart.
}
\usage{
Disdiam(obj, ...)

Silhouette(obj, ...)
}
\arguments{
  \item{ obj }{
    An object inherting from class \code{"VegsoupPartition"}.
  }
  \item{ \dots }{
    Arguments passed to optpart::disdiam.
  }
}
\details{
\code{Disdiam} is a cluster validation routine. See \code{\link[optpart:disdiam]{disdiam}} for details.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Interface for vegsoup to function \code{disdiam} by Dave Roberts provided with package \pkg{optpart}.
}
\seealso{
\code{\link{Silhouette}}
}
\keyword{ methods }

