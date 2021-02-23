\encoding{UTF-8}

\name{optimize}
\docType{methods}

\alias{optsil}
\alias{optsil-methods}
\alias{optsil,VegsoupPartition-method}

\alias{optindval}
\alias{optindval-methods}
\alias{optindval,VegsoupPartition-method}

\alias{remos}
\alias{remos-methods}
\alias{remos,VegsoupPartition-method}

\title{ Optimize Partitioning }

\description{
Modify the partitioning of an object as to optimize silhouette widths (distance based methods) or Dufrene and Legendre's indicator value (original data).
}

\usage{
\S4method{optsil}{VegsoupPartition}(x, maxitr = 100, verbose = FALSE, ...)

\S4method{optindval}{VegsoupPartition}(x, maxitr = 100, minsiz = 5, verbose = FALSE, ...)

\S4method{remos}{VegsoupPartition}(x, lim = -0.001, method = 2, maxitr = Inf, verbose = FALSE, ...)
}

\arguments{
  \item{ x }{
    \code{'VegsoupPartition'} object.
  }
  \item{ maxitr }{
    integer. The maximum number of iterations to perform.
  }
  \item{ verbose }{
	logical. Print elapsed CPU time.
  } 
  \item{ lim }{
    integer. A threshold of silhouette width for misclassified objects. It is basically close to zero but can be changed to any value between -1 and 0.
  }
  \item{ method }{
    integer. 1 for REMOS1, 2 for REMOS2, defaults to 2.
  }
  \item{ minsiz }{
    integer. The minimum size of the partition to consider reassigning a sample out of.
  }
  \item{ \dots }{
    arguments passed to \code{\link{as.dist}} (\code{optsil}) or
    \code{link{as.matrix}} (\code{optindval}).
  }
}

\details{
\code{optsil} is a simple wrapper for function \code{\link[optpart:optsil]{optsil}} in package \pkg{optpart} to polish an existing clustering.
%% Argument \code{maxitr} can be set to a lower value to speed up calculations, but should be set to a higher value.

\code{optindval} interfaces function \code{\link[optpart:optindval]{optindval}} in package \pkg{optpart}. This method maximizes the Dufrene and Legendre's indicator value (Indval).

\code{remos} interfaces the REMOS1 and REMOS2 algorithms according to Lengyel et al. 2021.
}

\value{
Returns a modified object of class \code{'VegsoupPartition'} with optimized classification. This means an object having a possibly changed partitioning vector.
}

\references{
Dufrene, M. and Legendre, P. 1997 Species assemblages and indicator species: The need for a flexible asymmetrical approach. \emph{Ecological Monographs}, \bold{67},345--366.

Lengyel, A., Roberts, D.W., Botta-Dukát, Z. Comparison of silhouette-based reallocation methods for vegetation classification. \emph{Journal of Vegetation Science}, \bold{DOI:10.1111/jvs.12984}.

Rousseeuw, P. 1987 Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, \bold{53},53--65.

Roberts, D. 2015 Vegetation classification by two new iterative reallocation optimization algorithms. \emph{Plant Ecology, 216},741–758.
}

\author{ Roland Kaiser, implemented for vegsoup using Dave W. Robert's \code{optsil} and \code{optindval} procedures of package \pkg{optpart} and Attila Lengyel's REMOS method.
}

\note{
\code{optsil} and \code{optindval} can be very slow when applied to big data sets! \code{optsil} is usually faster. \code{remos(x, method = 2)} is the most time efficient algrithm.
}

\examples{
require(vegsoup)
data(windsfeld)

x <- VegsoupPartition(windsfeld, k = 5)

xs <- optsil(x, verbose = TRUE)
xi <- optindval(x, verbose = TRUE)
confusion(xs, xi)

xr <- remos(x, verbose = TRUE)
confusion(xs, xr)

x <- coenoflex(500, 300)
x <- VegsoupPartition(x, k = 10)
xr <- remos(x, verbose = TRUE)
confusion(x, xr)

op <- par(mfrow = c(1, 2), pty = "s")
plot(silhouette(x))
plot(silhouette(xr))
par(op)
}
\seealso{
\code{\link[labdsv:indval]{indval}},
\code{\link[cluster:silhouette]{silhouette}}
}
\keyword{ methods }
\keyword{ cluster }

