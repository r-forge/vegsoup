\name{validate}
\docType{methods}

\alias{tabdev}
\alias{tabdev-methods}
\alias{tabdev,VegsoupPartition-method}

\alias{partana}
\alias{partana-methods}
\alias{partana,VegsoupPartition-method}

\alias{disdiam}
\alias{disdiam-methods}
\alias{disdiam,VegsoupPartition-method}

\alias{silhouette}
\alias{silhouette,VegsoupPartition-method}

\title{ Partition Quality Assessment for \code{"VegsoupPartition"} Objects }
\description{
  Partition analysis (\code{Partana}) evaluates the within-cluster to
  among-cluster similarity as a measure of cluster validity. Silhouette
  information (\code{Silhouette}) or Dissimilarity Diameters (\code{Disdiam})
  can also be used to evaluate a partitioning of an object. All three are
  distance based methods.

  Table Deviance (\code{Tabdev}) assess the clarity of the partitioning by
  permuting the columns (species) of the community matrix (original data) and
  calculates the probability of observing as low a deviance as observed.
 
  All methods are interfaces to corresponding functions in package
  \pkg{optpart}.
}

\usage{
\S4method{silhouette}{VegsoupPartition}(x, ...)

\S4method{partana}{VegsoupPartition}(x, verbose = FALSE, ...)

\S4method{disdiam}{VegsoupPartition}(x, ...)

\S4method{tabdev}{VegsoupPartition}(x, numitr = 99, verbose = FALSE, ...)
}

\arguments{
  \item{ x }{
    A \code{"VegsoupPartition"} object.
  }
  \item{ numitr }{
    Number of iterations to perform in calculating the probability of
    obtaining as effective a classification as observed.
  }
  \item{ verbose }{
  additional console output.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{as.dist}} (\code{Partana},
    \code{Silhouette} and \code{Disdiam}) or \code{link{as.matrix}}
    (\code{Tabdev}).
  }
}
\details{
\code{Partana} (\emph{partition analysis}) evaluates the within-cluster to among-cluster similarity of partitions as a measure of cluster validity. See \code{\link[optpart:partana]{partana}} for details. \code{\link[optpart:disdiam]{disdiam}} is also a cluster validation routine.

As opposed to \emph{partition analysis} (\code{Partana}) \code{Tabdev} is a method to assess the quality of partitions with respect to the original data, and not its distance matrix representation. See \code{\link[optpart:tabdev]{tabdev}} for details.

}
\value{
  \code{Partana} returns an S3 object of class \code{"partana"}.

  \code{Tabdev} returns an S3 object of class \code{"tabdev"}, a list with two
  elements \code{'spcdev'} and \code{'totdev'}.

  \item{ spcdev }{
    \code{data.frame} with four columns giving the species (\code{"numocc"}),
    the number of occurrences of a species in the data set (\code{"numocc"}),
    estimated deviance \code{"deviance"} and probability (\code{pval}).
  }
  \item{ totdev }{
    total tabele deviance.
  }
}
\author{
  Interfaced by Roland Kaiser for package \pkg{vegsoup} using \emph{David W.
  Roberts} \code{tabdev}, \code{partana}, \code{disdiam} and \code{silhouette}
  algorithms in package \pkg{optpart}.
}

\seealso{
\code{\link{optsil}},
\code{\link{optindval}}
}
\note{
To prevent 
}
\examples{
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

\dontrun{
## wait until optpart registers S3methods
silhouette(x)
disdiam(x)
tabdev(x)
partana(x)
}
}
\keyword{ methods }

