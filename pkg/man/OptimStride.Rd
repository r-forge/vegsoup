\name{OptimStride}
\alias{OptimStride}
\title{ Compute a Series of Partitions/Clusterings }
\description{
Compute a series of clusterings by differnet methods and quanitfy their quality using a Fisher test or table deviance.
}
\usage{
OptimStride(obj, k, ft.treshold = 1e-3, alternative = "two.sided",
            method = c("ward", "flexible", "pam", "kmeans", "wards"),
            CALL = match.call(), ...)
}
\arguments{
  \item{ obj }{
    an object of class \code{VegsoupPartition}
  }
  \item{ k }{
    maximum number of cluster to compute (\code{seq(1:k)})
  }
  \item{ ft.treshold }{
    treshold value of Fisher test, defaults to 1e-03
  }
  \item{ alternative }{
    alternative indicates the alternative hypothesis of the Fisher exact test
    and must be one of "two.sided", "greater" or "less" (see
    \code{\link{FisherTest}} and \link{fisher.test}).
  }
  \item{ method }{
    any method supported by \code{\link{VegsoupPartition}}. If missing "ward",
    "flexible", "pam", "kmeans", and "wards" methods are computed,
    computationally expensive methods, such as "isopam" and "optpart" are
    omitted to reduce run duration.
  }
  \item{ CALL }{
    To be removed.
  }
  \item{ \dots }{
    arguments passed to \code{VegsoupPartition} for additional arguments and
   details below.)
  }
}
\details{
The implementaion follows the method defined in Tíchý et al. (2010). The function takes several arguments passed to \code{VegsoupPartition}. For example \code{decostand.method} allows for all standardization methods available in \code{\link{decostand}}. \code{binary = TRUE} (the default) uses binary data only. \code{alternative} set the alternative hypothesis ind \code{\link{FischerTest}}.

}
\references{
Tíchý, L., Chytrý, M., Hájek, M., Talbot, S.S., and Botta-Dukát, Z. (2010).
Optimclass: Using species-to-cluster fidelity to determine the optimal
partition in classification of ecological communities. Journal of Vegetation
Science, 21(2):287–299.
}
\seealso{
\code{\link{VegsoupPartition}}
}
\examples{
require(vegsoup)
data(bigtestdata)
dta <- Vegsoup(spc.big, sts.big, txa.big, coverscale = "braun.blanquet")

res <- OptimStride(dta, k = 5)
class(res)

# plot method for class VegsoupOptimstride
plot(res)
}
\keyword{ multivariate }
