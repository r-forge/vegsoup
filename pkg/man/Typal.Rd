\name{typal}
\alias{typal}
\alias{typal-methods}

\title{
Typical Samples of an Object of Class Vegsoup*
}
\description{
Identifies samples (plots) that typify clusters in a partition based on dissimilarity.
}
\usage{
## S4 method for signature 'VegsoupPartition'
typal(obj, k = 1)
}
\arguments{
  \item{ obj }{
    An object inheriting from class \code{VegsoupPartition}. 
  }
  \item{ k }{
    integer. Number of typal species/clusters to identify. Defaults to 1.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{as.dist}}.
  }
}
\details{
Silhouette analysis returns \code{k} samples with the largest positive silhouette width for each partition. Partana analysis returns the \code{k} samples with the highest mean similarity to the partition. See \code{\link[optpart:typal]{typal}} in package \pkg{optpart} for further details.
}
\value{
Returns a \code{list} with two components
\item{ partana }{
A \code{data.frame} with the clusters as rows and typal samples as columns.
}
\item{ silhouette }{
A \code{data.frame} as above.
}

}
\author{
A wrapper for function \code{typal} by David W. Roberts in \pkg{optpart}.
}
\seealso{
\code{\link{as.dist}}
}
\examples{
library(vegsoup)
data(testdata)
        
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
prt <- VegsoupPartition(dta, k = 2)

## broken, due to as.dist dispatch
#typal(prt)
#typal(prt, typeof = "logical")
#typal(prt, typeof = "logical", mode = "R") 
}
\keyword{ methods }
