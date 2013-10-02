\name{validate}
\docType{methods}
\alias{Tabdev}
\alias{Tabdev-methods}
\alias{Partana}
\alias{Partana-methods}
\alias{Disdiam}
\alias{Disdiam-methods}
\alias{Silhouette}
\alias{Silhouette-methods}

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
\S4method{Silhouette}{VegsoupPartition}(obj, ...)

\S4method{Partana}{VegsoupPartition}(obj, verbose = FALSE, ...)

\S4method{Disdiam}{VegsoupPartition}(obj, ...)

\S4method{Tabdev}{VegsoupPartition}(obj, numitr = 99, verbose = FALSE, ...)
}

\arguments{
  \item{ obj }{
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
\code{\link{Optsil}},
\code{\link{Optindval}}
}
\note{
To prevent 
}
\examples{
library(vegsoup)
data(testdata)
prt <- VegsoupPartition(Vegsoup(spc, sts, txa, "braun.blanquet"), k = 2)
Tabdev(prt)
Partana(prt)
}
\keyword{ methods }
