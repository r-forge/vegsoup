\encoding{UTF-8}

\name{VegsoupPartition}

\alias{VegsoupPartition}

\title{ Partition a Vegsoup object }

\description{
  Create an object of class \code{'VegsoupPartition'} by selecting a predefined clustering algorithm or by supplying a function.
}

\usage{
VegsoupPartition(obj, k, method = c("ward", "flexible", "pam", "isopam",
                 "kmeans", "optpart", "wards", "fanny", "FCM", "KM", "external"),
                 clustering, polish = FALSE, seed = 1234, verbose = FALSE,
                 ...)
}

\arguments{
  \item{ obj }{
    \code{Vegsoup} object.
  }
  \item{ k }{
    number of partitions to create. Note, argument \code{k} can also be 1. In this case the whole object
    forms a single partition, the partitioning is flat.
  }
  \item{ method }{
    clustering method. See \sQuote{Details}.
  }
  \item{ clustering }{
    external vector of cluster partitons, or column to be selected from \code{obj}. See
    \code{\link{names}}.
  }
  \item{ polish }{
    apply optimization of silhouette widths \code{\link{optsil}} to merge singletons
    into an existing multi element partition. Note, this will likely decrease the initial number of
    \code{k}!
  }
  \item{ seed }{
    set random number generator to make results from methods \code{"kmeans"} and \code{"fanny"}
    reproducible.
  }
  \item{ verbose }{
    print diagnostic messages.
  }
  \item{ \dots }{
    passed to methods. Currently not supported!
  }
}

\details{
  The function creates a partitioned object of class \code{'Vegsoup'} using several clustering methods.
  All methods except \code{"kmeans"} take a \code{'dist'} object as data argument (\code{as.dist(obj)}).
  \code{"kmeans"} uses the original data matrix (\code{as.matrix(obj)}), whereas method \code{"KM"}
  is capable of using a \code{'dist'} objects. Specific implementation details are given below:

  \describe{
    \item{ ward }{
    calls \code{cluster::agnes} with \code{method = "ward"}.
    }
    \item{ flexible }{
    calls \code{cluster::agnes} with \code{method = "flexible"} and
    \code{par.method = c(alpha, alpha, beta, 0)}, where \code{alpha = 0.625} and
    \code{beta = 1 - 2 * alpha}.
    }
    \item{ pam }{
    calls \code{cluster::pam} with no specific additional options, except argument \code{k}.
    }
    \item{ kmeans }{
    calls \code{stats::kmeans} with \code{centers = k}. It doesn't use a dissimilarity matrix and proper
    standardization is required (e.g. Hellinger-distances, \code{decostand(obj) <- "hellinger"} and
    \code{vegdist(obj) <- "euclidean"}). In most cases method \code{"KM"} is preferred.
    }
    \item{ wards }{
    calls \code{stats::hclust} with \code{method = "ward"}.
    }
    \item{ fanny }{
    calls \code{cluster::fanny} with \code{k = k}, argument \code{memb.exp} is set to \code{1.1}
    if not supplied.
    }
    \item{ FCM, KM }{
    calls \code{vegclust::vegclustdist} with \code{mobileMemb = k} and \code{method = "FCM"},
    respectively \code{"KM"}. If you need further argument use the function interface (see FUN below).
    }
    \item{ isopam }{
    calls \code{isopam::isopam} using the \code{'distance'} argument
    }
    \item{ external }{
    simply takes a user (object) supplied partitioning vector \code{'clustering'}. If argument
    \code{'clustering'} is a \code{"character"} of length 1 the function seeks to select the supplied
    column from \code{obj}. If it is given as a vector of mode \code{numeric} and of length equal to
    \code{nrow(obj)} it is treated as the partitioning vector.this will be used. Vectors of mode
    \code{character} will be coerced using \code{as.numeric(factor(clustering))} to construct the
    clustering vector. In both cases argument \code{'method'} is ignored and will be set to
    \code{"external"}.
    }
    \item{ FUN }{
	If method is a function, \code{VegsoupPartition} will internally create a distance matrix
	by calling \code{Xd <- as.dist(obj)}. The raw data matrix, possibly standardized,
	is available with \code{X <- as.matrix(obj)}. Argument \code{k} is queried from the
	function's argument \code{k}. These object are evaluated with FUN by a call to
	\code{do.call(FUN, args = list(Xd = Xd, X = X, k = k))}. All other arguments
	must are defined inside FUN. The output of FUN needs to be nothing but a partitioning vector
	of integers!
	}	
  }
  
  Depending on the applied method the partitioning vector is obtained as follows: \code{'agnes'} and
  \code{'hclust'} objects are sliced by subsequent calls to \code{\link{cutree}} to obtain \emph{k}
  partitions. Objects of class \code{'pam'}, \code{'fanny'} and \code{'kmeans'} already contain a
  cluster(ing) attribute. If a hierarchy was estimated for \code{'isopam'} objects the flat attribute
  is accessed, otherwise the lowest hierarchy level is extracted. When using the function interface to
  \code{method} the returned partitioning vector of FUN should not be reordered in any way. The function
  must not assign names to the results vector, this is performed automatically.

  If \code{polish} is \code{TRUE} the function tries to groom singletons (single member group) using the
  \code{\link{optsil}} procedure. This may take some time to complete. Argument \code{maxitr} is set to
  \code{k^2}.
}

\section{Warning}{
  Method \code{"fanny"} and \code{fast = TRUE} may hang due to warnings
  issued by fanny and may freeze your R session. Setting \code{memb.exp = 1.1} will in most cases
  avoid any warnings of fanny.
}

\value{
  Returns an object of class \code{VegsoupPartition}. For method fanny the matrix containing the
  memberships is retained in the sites data (additional k columns are added).
}

\references{
  Missing, there are many!
}

\author{ Roland Kaiser }

\seealso{
  \code{\linkS4class{VegsoupPartition}}
  \code{\link{OptimStride}}
  \code{\link{hclust}}
}

\examples{
require(vegsoup)

# simulate 100 plots and 200 species distributed along a two-dimensional gradient
data(sim100)

r <- VegsoupPartition(sim100, k = 2, method = "fanny", memb.exp = 1.5)
Partitioning(r)
head(Sites(r))

# plot membership values against gradient values
stars(Sites(r)[grep("M", names(r))],
      locations = Sites(r)[, grep("X", names(r))] * 20, len = 1,
      draw.segments = TRUE, labels = NULL)
}

\keyword{ multivariate }

