\name{StablePartition}
\alias{StablePartition}

\title{ Classification Stability for \code{VegsoupPartition} Objects }
\description{
  Adjusted Goodman-Kruskal's lambda index of classification stability
  calculated from cross-tabulation of partitions (clusters) obtained from the
  data set and the subset selected by without replacement bootstrap.
}
\usage{
  StablePartition(obj, nitr = 200, nitr.lambda = 10, ...)
}

\arguments{
  \item{ obj }{
    an object of class \code{\link{VegsoupPartition}}
  }
  \item{ nitr }{
    number of iterations
  }
  \item{ nitr.lambda }{
    number of iteration to calculate random values of lambda
  }
  \item{ \dots }{
    arguments passed to \code{\link{VegsoupPartition}}.
  }
}
\details{
  Some more details here!
}
\value{
  returns a list with components
  \item{lambda}{
    lambda statistic
  }
  \item{modified.lambda}{
    modified lambda statistic
  }
}
\references{
adopted for vegsoup from script Classification Stability supplied for the
JUICE software (\url{http://www.sci.muni.cz/botany/juice/})
}
\author{ original authors, Roland Kaiser }
\seealso{
\code{\link{OptimStride}},
\code{\link{SampleVegsoup}},
}
\note{
  This function is experimental!
}
\examples{
data(bigtestdata)

dta <- Vegsoup(spc.big, sts.big, txa.big, coverscale = "braun.blanquet")

prt <- VegsoupPartition(dta, k = 5)

StablePartition(prt, nitr = 100, nitr.lambda = 10)
}

\keyword{ multivariate }
