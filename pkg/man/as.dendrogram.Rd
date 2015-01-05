\encoding{UTF-8}

\name{dendrogram}
\docType{methods}

\alias{as.hclust}
\alias{as.hclust,VegsoupPartition-method}

\alias{as.dendrogram}
\alias{as.dendrogram,VegsoupPartition-method}


\title{
Dendrograms of partition contingency tables.
}
\description{
The functions compute a contingency table (\code{\link{contingency}(object)}) and call \code{\link{hclust}} on this matrix.
}
\usage{
\S4method{as.hclust}{VegsoupPartition}(x, table = "constancy", ...)

\S4method{as.dendrogram}{VegsoupPartition}(object, table = "constancy", ...)
}
\arguments{
  \item{ x, object }{
    \code{VegsoupPartition} object. 
  }
  \item{ table }{
    character. Either \code{\link{contingency}}, \code{\link{constancy}} or
     \code{\link{average}}.
  }
  \item{ \dots }{
    Arguments passed to \code{vegan::vegdist}.

  }
}
%\details{
%Nothing here yet.
%}

\value{
\code{\link{hclust}} or \code{\link{dendrogram}} object.
}

\author{
   Roland Kaiser
}

\seealso{
\code{\link{VegsoupPartition}}
}

\examples{
require(vegsoup)

data(windsfeld)
x <- VegsoupPartition(windsfeld, k = 10)

par(mfrow = c(3,3))

# vegdist defaults to euclidean
vegdist(x)
 
plot(as.hclust(x, "constancy"))
plot(as.hclust(x, "average"))
plot(as.hclust(x, "contingency"))

# resetting invokes recalculation of the partitioning vector!
# we will regenreate it afterwards
p <- Partitioning(x)
vegdist(x) <- "bray"
x <- VegsoupPartition(x, method = "external", clustering = p)

plot(as.hclust(x, "constancy"))
plot(as.hclust(x, "average"))
plot(as.hclust(x, "contingency"))

# set method in hclust
plot(as.hclust(x, "constancy", "single"))
plot(as.hclust(x, "constancy", "complete"))
plot(as.hclust(x, "constancy", "average"))

}

\keyword{ methods }

