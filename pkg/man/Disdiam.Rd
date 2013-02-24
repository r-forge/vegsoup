\name{Disdiam}
\docType{methods}
\alias{Disdiam}
\alias{Disdiam-methods}
\title{ Dissimilarity Diameters of a Partition }
\description{
Calculates the diameter (maximum within-cluster dissimilarity) of all partitions, as well as the average diameter across all partitions using function \code{\link[optpart:disdiam]{disdiam}} of package \pkg{optpart}.
}
\usage{
Disdiam(obj, ...)
}
\arguments{
  \item{ obj }{
    An object inherting from class \code{'VegsoupPartition'}.
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
Interface for vegsoup to function \code{disdiam} by Dave Roberts provided with package optpart.
}
\seealso{
\code{\link{Silhouette}}
}
\keyword{ methods }

