\name{Vegsoupartition}
\alias{VegsoupPartition}

\title{ Partition/Cluster a \code{Vegsoup*} object}
\description{
Create an object of class \code{"VegsoupPartition"} by selecting a clustering algorithm.
}
\usage{
VegsoupPartition(obj, k, method = c("ward", "flexible", "pam", "isopam",
                 "kmeans", "optpart", "wards", "external"), clustering,
                 polish = FALSE, seed = 1234, verbose = FALSE,
                 ...)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ k }{
    number of partitions to create.
  }
  \item{ method }{
    clustering method. See \sQuote{Details}.
  }
  \item{ clustering }{
    external vector of cluster partitons, or column to be selected from
    Sites(obj)
  }
  \item{ polish }{
    apply optimal silhouette widths clustering \code{\link{Optsil}} on
    possible singletons (single element partitions) to sweep them to an
    existing multi element partition
  }
  \item{ seed }{
    set random number generator to make results from \code{method = "kmeans"}
    reproducible
  }
  \item{ verbose }{
    additional console output.
  }
  \item{ \dots }{
    passed to methods
  }
}
\details{
Create a partitioned object of class Vegsoup using several clustering methods.

%%Elaborate on methods!

If \code{clustering} is of length 1 and of mode \code{"character"} the function seeks to select the supplied column from \code{Sites(obj)}. If \code{clustering} is given as a vector of mode \code{numeric} and \code{length(clustering) == nrow(obj)} this will be used. A vector of mode\code{character} is coereced using \code{as.numeric(factor(clustering))} to construct the clustering vector. In both cases \code{method} will be set to \code{"external"}.

If \code{polish} is \code{TRUE} the function tries to groome singletons (single member group) using the \code{\link{Optsil}} procedure. This may take some time to complete. Argument \code{maxitr} is set to \code{k^2}.
}
\value{
Returns an object of class \code{VegsoupPartition}
}
\references{
Missing, there are many!
}
\author{ Roland Kaiser }
\note{
Argument \code{k} can also be 1. In this case the whole objects form a single
partition.
}
\seealso{
\code{\linkS4class{VegsoupPartition}}
\code{\link{OptimStride}}
\code{\link{hclust}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
