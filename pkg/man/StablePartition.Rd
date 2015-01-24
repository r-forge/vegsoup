\encoding{UTF-8}

\name{stable}

\alias{stable}
\alias{stable,VegsoupPartition-method}

\title{ Classification Stability }

\description{
  Adjusted Goodman-Kruskal's lambda index of classification stability
  calculated from cross-tabulation of partitions (clusters) obtained from the
  data set and the subset selected by without replacement bootstrap.
}
\usage{
\S4method{stable}{VegsoupPartition}(x, nitr = 200, nitr.lambda = 10, ...)
}

\arguments{
  \item{ x }{
    \code{\link{VegsoupPartition}} object.
  }
  \item{ nitr }{
    number of iterations.
  }
  \item{ nitr.lambda }{
    number of iteration to calculate random values of lambda.
  }
  \item{ \dots }{
    arguments passed to \code{\link{VegsoupPartition}} except argument \code{method}
    which is obtained from the object \code{x}.
  }
}
\details{
  Some more details here!
}
\value{
  returns a list with components
  \item{ lambda }{
    lambda statistic
  }
  \item{ modified.lambda }{
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
\code{\link{sample}},
}
\note{
  This function is experimental!
}
\examples{
require(vegsoup)

data(windsfeld)
x <- VegsoupPartition(windsfeld, k = 10)

stable(x, nitr = 100, nitr.lambda = 10)
}

\keyword{ multivariate }

