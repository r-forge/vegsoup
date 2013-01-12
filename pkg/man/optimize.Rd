\name{optimize}
\docType{methods}
\alias{Optsil}
\alias{Optsil-methods}
\alias{Optsil,VegsoupDataPartition-method}
\alias{Optindval}
\alias{Optindval-methods}
\alias{Optindval,VegsoupDataPartition-method} 
\title{
Optimise Partitioning of an Object of Class VegsoupDataPartition
}
\description{
Modify the partitioning of an object as to optimze silhouette widths or Dufrene and Legendre's indicator value.
}
\usage{
Optsil(obj, maxitr = 100, verbose = FALSE, ...)

Optindval(obj, maxitr = 100, minsiz = 5, verbose = FALSE, ...) 
}

\arguments{
  \item{obj}{
An object of class \code{"VegsoupDataPartition"}.
  }
  \item{ maxitr }{
The maximum number of iterations to perform.
  }
  \item{ verbose }{
Print cpu time used.
  } 
  \item{ minsiz }{
The minimum size of the partition to consider reassigning a sample out of.
  }
  \item{\dots}{
Arguments passed to \code{link{as.dist}} (\code{optsil}) or \code{link{as.matrix}} (\code{Optindval}).
  }
}

\details{
\code{Optsil} is a simple wrapper for function \code{\link[optpart:optsil]{optsil}} in package \pkg{optpart} to polish an existing clustering. Argument \code{maxitr} is set to one by default to speed up calculations, but should be set to a higher value.

\code{Optindval} interfaces function \code{\link[optpart:optindval]{optindval}} in package \pkg{optpart}. This method maximises the Dufrene and Legendre's indicator value (Indval).
}

\value{
Returns a modified object of class \code{"VegsoupDataPartition"} with possibly changed partitioning vector.
}

\references{
Dufrene, M. and Legendre, P. (1997). Species assemblages and indicator species: The need for a flexible asymmetrical approach. Ecological Monographs, 67(3):345–366.

Rousseeuw, P. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. Journal of Computational and Applied Mathematics, 53:53–65.
}

\author{
Implemented for vegsoup using Dave Roberts \code{optsil} and \code{optindval} procedures.
}

\note{
Both methods can be very slow when applied to big data sets! \code{Optsil} is usually faster.
}

\examples{
library(vegsoup)
data(bigtestdata)

dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))
prt <- VegsoupDataPartition(dta, k = 4)

prt.sil <- Optsil(prt, verbose = TRUE)
prt.ind <- Optindval(prt, verbose = TRUE)

Confus(prt, prt.opt)
Confus(prt, prt.ind)	
}
\seealso{
\code{\link[labdsv:indval]{indval}},
\code{\link[cluster:silhouette]{silhouette}}
}


\keyword{ methods }
\keyword{ cluster }
