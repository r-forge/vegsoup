\name{validate}
\docType{methods}
\alias{Tabdev}
\alias{Tabdev-methods}
\alias{Partana}
\alias{Partana-methods}

\title{ Partition Quality Assessment for \code{"VegsoupPartition"} Objects }
\description{
Partition analysis (\code{partana}) evaluates the within-cluster to among-cluster similarity as a measure of cluster validity (distance based method). Table Deviance (\code{Tabdev}) assess the clarity of the partitioning by permuting the columns (species) of the community matrix (original data) and calculates the probability of observing as low a deviance as observed. Both methods are interfaces to corresponding functions in package \pkg{optpart}.
}
\usage{
Tabdev(obj, numitr = 999, verbose = FALSE, ...)
Partana(obj, verbose = FALSE, ...)
}

\arguments{
  \item{ obj }{
    An object of class \code{"VegsoupPartition"}
  }
  \item{ numitr }{
    Number of iterations to perform in calculating the probability of
    obtaining as effective a classification as observed.
  }
  \item{ verbose }{
  additional console output.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{as.dist}} (\code{Partana}) or
    \code{link{as.matrix}} (\code{Tabdev}).
  }
}
\details{
\code{Partana} (\emph{partition analysis}) evaluates the within-cluster to among-cluster similarity of partitions as a measure of cluster validity. See \code{\link[optpart:partana]{partana}} for details.

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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Interfaced by Roland Kaiser for package \pkg{vegsoup} using David W. Roberts
\code{tabdev} and \code{partana} algorithms in package \pkg{optpart}.
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
