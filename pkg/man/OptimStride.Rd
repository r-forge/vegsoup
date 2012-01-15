\name{OptimStride}
\alias{OptimStride}
\title{
  Compute a series of clusterings and quanitfy their quality
}
\description{
  Compute a series of clusterings and by differnet clustering methods and quanitfy their quality using indicator sopecies analysis following the method defined in Tíchý et al. 2010.
}
\usage{
OptimStride(obj, k, ft.treshold = 1e-3, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards"), CALL = match.call(), ...)
}
\arguments{
  \item{obj}{an object of class \code{\link{VegsoupDataPartition}}}
  \item{k}{maximum number of cluster to compute (\code{seq(1:k)})}
  \item{ft.treshold}{treshold value of Fisher test, defaults to 1e-03}
  \item{alternative}{alternative indicates the alternative hypothesis of Fisher exact test (\link{FisherTest}) and must be one of "two.sided", "greater" or "less" (see \code{\link{FisherTest}} and \link{fisher.test}).}
  \item{method}{any method supported by \code{\link{VegsoupDataPartition}}. If missing "ward", "flexible", "pam", "kmeans", and "wards" methods are computed, computationally expensive methods, such as "isopam" and "optpart" are omitted by default.}
  \item{\dots}{arguments passed to VegsoupDataPartition (see \code{\link{VegsoupDataPartition}} for additional arguments and details below.)}
}
\details{
The function takes several arguments passed to \code{\link{VegsoupDataPartition}}. For example \code{decostand.method} allows for all standardization methods available in \code{\link{decostand}}. \code{binary = TRUE} (the default) uses binary data only. \code{alternative} set the alternative hypothesis ind \code{\link{FischerTest}}.

}
\references{
Tíchý, L., Chytrý, M., Hájek, M., Talbot, S. S., and Botta-Dukát, Z. (2010). Optimclass: Using species-to-cluster fidelity to determine the optimal partition in classification of ecological communities. Journal of Vegetation Science, 21(2):287–299.
}
\seealso{
%% \code{\link{VegsoupData-class}, \link{readOGR}}
}
\examples{

}
\keyword{ ~kwd }
