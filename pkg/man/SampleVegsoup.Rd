\encoding{UTF-8}

\name{sample}
\docType{methods}

\alias{sample}
\alias{sample,Vegsoup-method}

\alias{hcr}
\alias{hcr,Vegsoup-method}

\title{ Random Samples and Permutations for \code{Vegsoup*} Objects }

\description{
The methods take a sample of the specified size from the elements of \code{x} using either with or without replacement. Permutations and random subsamples of the input object can be performed with method \code{sample}. Heterogeneity-constrained random samples are obtained with method \code{hcr}.
}

\usage{
\S4method{sample}{Vegsoup}(x, size, replace = FALSE, prob = NULL)

\S4method{hcr}{Vegsoup}(x, size, nperm = 1000, fast = FALSE, ...)
}

\arguments{
  \item{ x }{
	\code{Vegsoup*} object.
  }
  \item{ size }{
	non-negative integer giving the number of items to choose. For \code{hcr} defaults to \code{floor(nrow(x) * 0.1)} and for \code{sample} to \code{nrow(x)}.
  }
  \item{ replace }{
	should sampling be with replacement?
  }
  \item{ prob }{
	vector of probability weights for obtaining the elements of the vector
	being sampled.
  }
  \item{ nperm }{
	the number of permutations.
  }
  \item{ fast }{
	accelerate computations using package \code{'parallel'}.
  }
  \item{ \dots }{
	additional arguments \code{replace} and \code{prob} passed to \code{\link{sample}}.
  }
}

\details{
See \code{\link{sample}} for details. Method \code{hcr} returns \emph{the most representative} subsample of specified size by selecting from \code{nperm} independent subsets the one that has lowest mean dissimilarity and highest variance.
}

\value{
  An object of the same class as the input object.
}
\author{ Roland Kaiser, method \code{hcr} is an re-implementation of function \code{hcr} in package \pkg{vegclust} by Miquel De \enc{Cáceres}{Caceres} that is capable of forking parallel processes. }

\references{
Lengyel, A., Chytry, M., Tichy, L. (2011). Heterogeneity-constrained random resampling of phytosociological databases. Journal of Vegetation Science 22: 175-183.
}
\seealso{
\code{\link[vegclust:hcr]{hcr}} in \pkg{vegclust}, \code{\linkS4class{Vegsoup}}, \code{\link{seriation}}
}

\examples{
library(vegsoup)
data(windsfeld)
r <- sample(windsfeld)
rownames(r) # order is permuted

rownames(sample(windsfeld, size = 3))
rownames(sample(windsfeld, replace = TRUE))

hcr(windsfeld, size = 20)

\dontrun{
system.time(hcr(windsfeld, size = 20, nperm = 100000, fast = FALSE))
system.time(hcr(windsfeld, size = 20, nperm = 100000, fast = TRUE))
system.time(vegclust::hcr(as.dist(windsfeld), nout = 20, nsampl = 100000))
}
}
\keyword{methods}

