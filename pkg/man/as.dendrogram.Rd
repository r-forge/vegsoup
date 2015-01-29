\encoding{UTF-8}

\name{dendrogram}
\docType{methods}

\alias{as.hclust}
\alias{as.hclust,VegsoupPartition-method}

\alias{as.dendrogram}
\alias{as.dendrogram,VegsoupPartition-method}

\alias{reorder}
\alias{reorder,VegsoupPartition-method}

\title{ Dendrograms of Partitions }

\description{
The functions compute a contingency table (\code{\link{contingency}(object)}) and call \code{\link{hclust}} on this matrix.
}

\usage{
\S4method{as.hclust}{VegsoupPartition}(x, table = "constancy", ...)

\S4method{as.dendrogram}{VegsoupPartition}(object, table = "constancy", labels = NULL, ...)

\S4method{reorder}{VegsoupPartition}(x, dendrogram, ...)
}

\arguments{
  \item{ x, object }{
    \code{VegsoupPartition} object. 
  }
  \item{ table }{
    character. Either \code{\link{contingency}}, \code{\link{constancy}} or
    \code{\link{average}}.
  }
  \item{ labels }{
    character. Column of \code{object} to label dendrogram. This must of course be in line with partitioning.
  }
  \item{ dendrogram }{
    dendrogram. Object to be used to reorder the partitions as to reflect the topology (order of tips) of a dendrogram.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{hclust}} and \code{\link{as.dendrogram}}, respectively.
  }
}

\details{
The \code{as*} methods coerce objects to the class indicated by their name.  
Plotting the topology of partitions can provide insights into relations of clusters to each other.  
As the \code{\link{VegsoupPartition}} constructor function looses the information that could be retained from hierarchical clustering methods, this is a way to rediscover parts of this information.

Method \code{reorder} uses the topology of a dendrogram to reorder the partitioning vector of an object.  
This can be useful to define an order for methods like \code{constancy} or \code{latex.}.  
}

\value{
\code{\link{hclust}}, \code{\link{dendrogram}}, or \code{VegsoupPartition} object.
}

\author{
   Roland Kaiser
}

\seealso{
\code{\link{VegsoupPartition}}
}

\examples{
x <- VegsoupPartition(windsfeld, k = 10)

par(mfrow = c(3,3))

# vegdist defaults to euclidean
vegdist(x)
 
plot(as.hclust(x, "constancy"))
plot(as.hclust(x, "average"))
plot(as.hclust(x, "contingency"))

# resetting invokes recalculation of the partitioning vector!
# we will regenreate it afterwards
p <- partitioning(x)
vegdist(x) <- "bray"
x <- VegsoupPartition(x, method = "external", clustering = p)

plot(as.hclust(x, "constancy"))
plot(as.hclust(x, "average"))
plot(as.hclust(x, "contingency"))

# set method in hclust
plot(as.hclust(x, "constancy", "single"))
plot(as.hclust(x, "constancy", "complete"))
plot(as.hclust(x, "constancy", "average"))

# as.dendrogram
par(mfrow = c(2,1))
plot(as.dendrogram(x))

# define labels for as.dendrogram
x$label <- LETTERS[partitioning(x)]
plot(as.dendrogram(x, labels = "label"))

# reorder partition slot with a dendrogram
# see the differnce using a constancy table.
head(constancy(x))

xx <- reorder(x, as.dendrogram(x))
head(constancy(xx))
}

\keyword{ methods }

