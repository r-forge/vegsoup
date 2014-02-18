\name{optimize}
\docType{methods}

\alias{Optsil}
\alias{Optsil-methods}
\alias{Optsil,VegsoupPartition-method}

\alias{Optindval}
\alias{Optindval-methods}
\alias{Optindval,VegsoupPartition-method}

\title{ Optimize Partitioning }

\description{
Modify the partitioning of an object as to optimize silhouette widths (distance based method) or Dufrene and Legendre's indicator value (original data).
}

\usage{
\S4method{Optsil}{VegsoupPartition}(obj, maxitr = 100, verbose = FALSE, ...)
\S4method{Optindval}{VegsoupPartition}(obj, maxitr = 100, minsiz = 5, verbose = FALSE, ...) 
}

\arguments{
  \item{ obj }{
    A \code{'VegsoupPartition'} object.
  }
  \item{ maxitr }{
    The maximum number of iterations to perform.
  }
  \item{ verbose }{
    Print CPU time used.
  } 
  \item{ minsiz }{
    The minimum size of the partition to consider reassigning a sample out of.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{as.dist}} (\code{Optsil}) or
    \code{link{as.matrix}} (\code{Optindval}).
  }
}

\details{
\code{Optsil} is a simple wrapper for function \code{\link[optpart:optsil]{optsil}} in package \pkg{optpart} to polish an existing clustering.
%% Argument \code{maxitr} can be set to a lower value to speed up calculations, but should be set to a higher value.

\code{Optindval} interfaces function \code{\link[optpart:optindval]{optindval}} in package \pkg{optpart}. This method maximises the Dufrene and Legendre's indicator value (Indval).
}

\value{
Returns a modified object of class \code{'VegsoupPartition'} with possibly changed partitioning vector.
}

\references{
Dufrene, M. and Legendre, P. (1997). Species assemblages and indicator species: The need for a flexible asymmetrical approach. Ecological Monographs, 67(3):345–366.

Rousseeuw, P. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. Journal of Computational and Applied Mathematics, 53:53–65.
}

\author{ Roland Kaiser, implemented for vegsoup using Dave W. Roberts \code{optsil} and \code{optindval} procedures of package \pkg{optpart}.
}

\note{
Both methods can be very slow when applied to big data sets! \code{Optsil} is usually faster.
}

\examples{
require(vegsoup)
data(windsfeld)

x <- VegsoupPartition(windsfeld, k = 5)

xs <- Optsil(x, verbose = TRUE)
xi <- Optindval(x, verbose = TRUE)

Confus(xs, xi)
}
\seealso{
\code{\link[labdsv:indval]{indval}},
\code{\link[cluster:silhouette]{silhouette}}
}
\keyword{ methods }
\keyword{ cluster }

