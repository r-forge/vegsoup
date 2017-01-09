\encoding{UTF-8}

\name{isolate}
\docType{methods}

\alias{isolate}
\alias{isolate,VegsoupPartition-method}

\title{ Manually Modifiy Partitions }

\description{
The function provides means to manually modify the partitioning vector.
}

\usage{
\S4method{isolate}{VegsoupPartition}(x, plot, ...)
}

\arguments{
  \item{ x }{
    \code{VegsoupPartition} object.
  }
  \item{ plot }{
    character. The plot(s) to force into a new partition.
  }
  \item{ \dots }{
    Currently not used.
  }

}

\details{
This function increases the value of \code{getK(x)}.
}

\value{
\code{code{VegsoupPartition}} object.
}

\author{
   Roland Kaiser
}

\seealso{
\code{\link{VegsoupPartition}}
}

\examples{
x <- VegsoupPartition(barmstein, k = 2)
x <- isolate(x, rownames(x)[1])

getK(x)
x
}

\keyword{ methods }

