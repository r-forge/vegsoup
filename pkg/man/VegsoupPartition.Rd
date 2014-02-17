\name{VegsoupPartition}

\alias{VegsoupPartition}

\title{ Partition/Cluster a \code{Vegsoup*} object}
\description{
Create an object of class \code{"VegsoupPartition"} by selecting a clustering algorithm.
}
\usage{
VegsoupPartition(obj, k, method = c("ward", "flexible", "pam", "isopam",
                 "kmeans", "optpart", "wards", "fanny", "FCM", "KM", "external"),
                 clustering, polish = FALSE, seed = 1234, verbose = FALSE,
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
    set random number generator to make results from methods \code{"kmeans"} and \code{"fanny"}
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

If \code{clustering} is of length 1 and of mode \code{"character"} the function seeks to select the supplied column from \code{Sites(obj)}. If \code{clustering} is given as a vector of mode \code{numeric} and \code{length(clustering) == nrow(obj)} this will be used. A vector of mode\code{character} is coerced using \code{as.numeric(factor(clustering))} to construct the clustering vector. In both cases \code{method} will be set to \code{"external"}.

If \code{polish} is \code{TRUE} the function tries to groom singletons (single member group) using the \code{\link{Optsil}} procedure. This may take some time to complete. Argument \code{maxitr} is set to \code{k^2}.
}
\note{
  If you gain errors using \code{instantsoup}, try to create separate objects
  for classes \code{"Species"}, \code{"Sites"} and \code{"Taxonomy"}. You may
  also want to call \code{"SpeciesTaxonomy"} for the first and last objects
  mentioned. Defaults for \code{sep} and \code{dec} passed to \code{read.csv}
  are set according to \code{read.csv2}!
}

\section{Warning}{
  Method \code{"fanny"} and \code{fast = TRUE} may hang due to warnings
  issued by fanny and may freeze your R session. Setting \code{memb.exp = 1.1} will in most cases
  avoid any warning of fanny.
}

\value{
Returns an object of class \code{VegsoupPartition}.
For method fanny the matrix containing the memberships is retained in the sites data.
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
\examples{
require(vegsoup)

# simulate 100 plots and 200 species distributed along a two-dimensional gradient
x <- coenoflex(100, 200)
r <- VegsoupPartition(x, k = 2, method = "fanny", memb.exp = 1.5)
Partitioning(r)
head(Sites(r))

# plot membership values against gradient values
stars(Sites(r)[grep("M", names(r))],
      locations = Sites(r)[, 1:2] * 20, len = 1,
      draw.segments = TRUE, labels = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
