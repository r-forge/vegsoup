\name{OptimStride}
\alias{OptimStride}

\alias{methods}
\alias{stride}
\alias{treshold}
\alias{optimclass1}
\alias{optimclass2}
\alias{peaks}
\alias{which.max}

\title{ Compute a Series of Partitions/Clusterings }

\description{
Compute a series of clusterings by different methods and quantify their quality using a Fisher test.
}

\usage{
OptimStride(obj, k, ft.treshold = 1e-3, alternative = "two.sided",
            method = c("ward", "flexible", "pam", "kmeans", "wards"),
            fast = FALSE, CALL = match.call(), ...)
\S4method{methods}{VegsoupOptimstride}(x)

\S4method{stride}{VegsoupOptimstride}(x, method, ...)

\S4method{treshold}{VegsoupOptimstride}(x, ...)

\S4method{optimclass1}{VegsoupOptimstride}(x)

\S4method{optimclass2}{VegsoupOptimstride}(x, treshold = 2)

\S4method{peaks}{VegsoupOptimstride}(x)

\S4method{which.max}{VegsoupOptimstride}(x)
}

\arguments{
  \item{ obj }{
    \code{VegsoupPartition} object
  }
  \item{ x }{
   \code{VegsoupOptimstride} object
  }
  \item{ k }{
    maximum number of cluster to compute (\code{seq(1:k)})
  }
  \item{ ft.treshold }{
    treshold value of Fisher test, defaults to 1e-03
  }
  \item{ treshold }{
    treshold value for optimclass2
  }
  \item{ alternative }{
    alternative indicates the alternative hypothesis of the Fisher exact test
    and must be one of "two.sided", "greater" or "less" (see
    \code{\link{FisherTest}} and \link{fisher.test}).
  }
  \item{ method }{
    any method supported by \code{\link{VegsoupPartition}}. For \code{OptimStride}
    if method is missing, "ward",  "flexible", "pam", "kmeans", and "wards" are computed,
    computationally expensive methods, such as "isopam" and "optpart" are
    omitted to reduce run duration.
  }
  \item{ fast }{
    accelerate computations. Needs package \code{multicore}. If \code{FALSE} a progress bar
    is printed. Depending on the dimension of the community matrix and the value of k, computations
    may consume a serious amount of time. If the host system provides 4 or more core, setting
    \code{fast = TRUE} will significantly reduce run time.
  }
  \item{ \dots }{
    arguments passed to \code{VegsoupPartition} for additional arguments and
    details below.)
  }
}
\details{
   The implementation follows the method defined in Tíchý et al. (2010) and uses a fisher test (\code{\link{FisherTest}})
   to identify the number of \sQuote{faithful} species. This statistic can be relaxed by setting a threshold value
   (argument \code{ft.treshold}). All specifications (method to calculate distance matrix, standardisation) are
   taken from the input object.
   
   \code{treshold} retrieves the threshold values set for the Fisher test.
   
   \code{stride} returns the number of faithful species for each partition and number of k.
   
   \code{optimclass1} and \code{optimclass2} returns a matrix of faithful species.
   
   \code{peaks} finds peaks in the stride
   
   \code{which.max} returns the number of k that coincides with the highest number of faithful species.
   
}

\value{
  For function \code{Optimstride} an \code{VegsoupOptimstride} object is returned.
  \code{methods} returns the computed methods as character.
  \code{stride} returns a list with the number of significant indicator species.
  If \code{length(methods(x))} is longer than one, a named list of lists is returned.
}

\references{
Tíchý, L., Chytrý, M., Hájek, M., Talbot, S.S., and Botta-Dukát, Z. (2010).
Optimclass: Using species-to-cluster fidelity to determine the optimal
partition in classification of ecological communities. Journal of Vegetation
Science, 21(2):287–299.
}

\seealso{
\code{\link{VegsoupPartition}}, \code{\link{Fidelity}}
}

\examples{
require(vegsoup)

# a dummy example using simulated data
# we don't assume to spot discrete communities in this case
x <- coenoflex(100, 200)

res <- OptimStride(x, k = 10)
summary(res)
# the methods computed
methods(res)
# the number of faithful species for each partition and method
stride(res)
stride(res, "felxible")
# the treshold used

# plot method for class VegsoupOptimstride
plot(res)
}
\keyword{ multivariate }
