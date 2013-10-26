\name{SampleVegsoup}
\alias{SampleVegsoup}

\title{ Random Samples and Permutations for \code{Vegsoup*} Objects }

\description{
The method takes a sample of the specified size from the elements of x using either with or without replacement.
}

\usage{
SampleVegsoup(x, size, replace = FALSE, prob = NULL)
}

\arguments{
  \item{ x }{
    a \code{Vegsoup*} object.
  }
  \item{ size }{
    a non-negative integer giving the number of items to choose.
  }
  \item{ replace }{
    Should sampling be with replacement?
  }
  \item{ prob }{
    A vector of probability weights for obtaining the elements of the vector
    being sampled.
  }
}

\details{
  See \code{\link{sample}} for details.
}

\value{
  An object of same class as the input class.
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{Vegsoup}}
\code{\link{seriation}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")
rownames(dta)
dta.smp <- SampleVegsoup(dta)
rownames(dta.smp) # order is permuted

rownames(SampleVegsoup(dta, size = 3))
rownames(SampleVegsoup(dta, replace = TRUE))
}
\keyword{methods}
