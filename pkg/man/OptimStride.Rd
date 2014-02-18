\encoding{UTF-8}
\docType{methods}

\name{OptimStride}
\alias{OptimStride}

\alias{methods}
\alias{methods,VegsoupOptimstride-method}

\alias{stride}
\alias{stride,VegsoupOptimstride-method}

\alias{threshold}
\alias{threshold,VegsoupOptimstride-method}

\alias{optimclass1}
\alias{optimclass1,VegsoupOptimstride-method}

\alias{optimclass2}
\alias{optimclass2,VegsoupOptimstride-method}

\alias{getK,VegsoupOptimstride-method}

\alias{peaks}
\alias{peaks,VegsoupOptimstride-method}

\alias{which.max}
\alias{which.max,VegsoupOptimstride-method}

\title{ Compute a Series of Partitions/Clusterings }

\description{
  Compute a series of clusterings (strides) by different methods and quantify their quality using
  a Fisher test.
}

\usage{
OptimStride(x, k, ft.threshold = 1e-3, alternative = "two.sided",
            method = c("ward", "flexible", "pam", "kmeans", "wards",
                       "fanny", "FCM", "KM"),
            fast = FALSE, ...)

\S4method{methods}{VegsoupOptimstride}(x, ...)

\S4method{stride}{VegsoupOptimstride}(x, method, ...)

\S4method{threshold}{VegsoupOptimstride}(x, ...)

\S4method{optimclass1}{VegsoupOptimstride}(x, ...)

\S4method{optimclass2}{VegsoupOptimstride}(x, threshold = 2, ...)

\S4method{getK}{VegsoupOptimstride}(x)

\S4method{peaks}{VegsoupOptimstride}(x, ...)

\S4method{which.max}{VegsoupOptimstride}(x)
}

\arguments{
  \item{ x }{
   for \code{OptimStride} a \code{'VegsoupPartition'} object, otherwise a
   \code{VegsoupOptimstride} object
  }
  \item{ k }{
    maximum number of cluster to compute (\code{seq(1:k)})
  }
  \item{ ft.threshold }{
    threshold value of Fisher test, defaults to \code{1e-03}
  }
  \item{ threshold }{
    threshold value for OptimClass2. See \sQuote{Details}
  }
  \item{ alternative }{
    alternative indicates the alternative hypothesis of the Fisher exact test
    and must be one of "two.sided", "greater" or "less" (see
    \code{\link{FisherTest}} and \link{fisher.test}).
  }
  \item{ method }{
    any method supported by \code{\link{VegsoupPartition}}. For \code{OptimStride}
    if method is missing, "ward", "flexible", "pam", "kmeans", "wards", "fanny", "FCM", and "KM"
    are computed. Computationally expensive methods, such as "isopam" and "optpart" are
    omitted to reduce run duration.
  }
  \item{ fast }{
    accelerate computations using package \code{parallel}. If \code{FALSE} a progress bar
    is printed. Depending on the dimension of the community matrix and the value of k, computations
    may consume a serious amount of time. If the host system provides 4 or more core, setting
    \code{fast = TRUE} will significantly reduce run time.
  }
  \item{ \dots }{
    additional arguments passed to \code{VegsoupPartition}, otherwise not used.
  }
}
\details{
  The implementation follows the method of Tichy et al. (2010) and uses a Fisher test
  (\code{\link{FisherTest}}) to identify the number of \sQuote{faithful} species. This sensitivity
  of this statistic can be controlled by setting a threshold value. Per default this value
  (argument \code{ft.threshold}) is set to it's lowest meaningful bound \code{1e-3}.
  
  Partitioning/clustering methods, type of distance matrix and standardization are taken from the
  input object. Generic methods for \code{'VegsoupOptimstride'} are detailed in the
  \sQuote{Methods} section.
  
}
\section{ Methods }{
  \describe{
    \item{ methods }{
      \code{ signature(obj = "VegsoupOptimstride") }:
         retrieves the names of the computed method(s). These are the method arguments as accepted
         by \code{VegsoupPartition}.
    }
    \item{ stride }{
      \code{ signature(obj = "VegsoupOptimstride") }:
        returns the number of faithful species for each partition and number of k.
    }
    \item{ threshold }{
      \code{ signature(obj = "VegsoupOptimstride") }:
         retrieves the threshold values set for the Fisher test.
    }
    \item{ optimclass1, optimclass2 }{
      \code{ signature(obj = "VegsoupOptimstride") }:
        returns a matrix with the counts of faithful species, those achieving
        \code{ft.trshold}.
    }
    \item{ getK }{
      \code{ signature(obj = "VegsoupOptimstride") }:
        the computed maximum value of \code{k}, the length of each stride.
    }
    \item{ peaks }{
      \code{ signature(obj = "VegsoupOptimstride") }:
        the peaks along the stride.
    }
    \item{ which.max }{
      \code{ signature(obj = "VegsoupOptimstride") }:
        returns the number of k that coincides with the highest number of faithful species,
        per method.
    }
  }
}

\value{
  \item{ Optimstride }{
  \code{VegsoupOptimstride} object
  }
  \item{ methods }{
  character. The computed methods.
  }
  \item{ stride }{
  list. A list with length equal to \code{length(methods(x))}. Each nested list gives the number
  of significant indicator species (faithful species) for each partition for a particular value
  of \code{k}.
  }
  \item{ optimclass1, optimclass2 }{
  matrix with dimnames. Rows are methods and columns are the respective values of \code{k}.
  }
  \item{ peaks,  }{
  }
}

\references{
Tichy, L., Chytry, M., Hajek, M., Talbot, S.S., and Botta-Dukat, Z. (2010). Optimclass: Using species-to-cluster fidelity to determine the optimal partition in classification of ecological communities. Journal of Vegetation Science, 21(2):287–299.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{VegsoupPartition}},\code{\link{Fidelity}}
}

\examples{
require(vegsoup)

# a dummy example using simulated data
x <- coenoflex(100, 200)

x <- OptimStride(x, k = 10, method = c("ward", "flexible", "pam"))

summary(x)

# use parallel package
\dontrun{
if (require(parallel)) {
   x <- OptimStride(coenoflex(100, 200), k = 10, fast = TRUE)
}
}

# get the computed clustering methods
methods(x)

# the threshold of the Fisher test
threshold(x)

# matrix of results for OptimClass1
optimclass1(x)

# the number of faithful species for each partition and method
# warning, method dispatch will break if package optpart is loaded
stride(x)
stride(x, method = "flexible") # for method flexible

# plot method for class VegsoupOptimstride
plot(x)
plot(x, method = "flexible")

# get k where the curve peaks
peaks(x)

# get k with the maximum number of faithful species
which.max(x)

}
\keyword{ methods }
\keyword{ multivariate }

