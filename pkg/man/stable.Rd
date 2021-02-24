\encoding{UTF-8}

\name{stable}

\alias{stable}
\alias{stable,VegsoupPartition-method}

\title{ Classification Stability }

\description{
  Adjusted Goodman-Kruskal's lambda index of classification stability
  calculated from cross-tabulation of partitions (clusters) obtained from the
  data set and the subset selected by without-replacement bootstrap
  (\enc{Tichý}{Tichy} et al. 2011). Assignments are evaluated by
  Goodman-Kruskal’s lambda index and a modified version of lambda.
}
\usage{
\S4method{stable}{VegsoupPartition}(x, nitr = 200, nitr.lambda = 10, seed = 1234, ...)
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
  \item{ seed }{
    to make things reproducible.
  }
  \item{ \dots }{
    arguments passed to \code{\link{VegsoupPartition}} except argument \code{method}
    which is obtained from the object \code{x}.
  }
}
\value{
  returns a list with components
  \item{ lambda }{
    lambda statistic, values range from 0 (no association) to 1 (the
    theoretical maximum possible association)
  }
  \item{ modified.lambda }{
    modified lambda statistic.
  }
  \item{ random.lambda }{
    random lambda statistic used to adjust raw lambda values.
  }
}
\references{
  Tichý, L. and Chytrý, M. and Šmarda, P. (2011) Evaluating the stability of
  the classification of community data. Ecography 34:807-813
}
\author{ adapted by Kaiser based on \R-code supplied with the JUICE software }
\seealso{
\code{\link{OptimStride}},
\code{\link{sample}}
}
\examples{
x <- VegsoupPartition(coenoflex(seed = 1234), k = 3)

stable(x, nitr = 19, nitr.lambda = 10)
}

\keyword{ multivariate }

