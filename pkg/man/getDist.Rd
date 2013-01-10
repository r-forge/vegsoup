\name{getDist}
\docType{methods}
\alias{getDist}
\alias{getDist-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a Distance Matrix from Objects of Class VegsoupData*
}
\description{
Compute a distance matrix as specified by the \code{dist} slot of an object by calls to \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.  
}
\usage{
getDist(obj)
}
\arguments{
  \item{obj}{
An object of class \code{VegsoupData*}.
}
  \item{binary}{
Compute the distance matrix based on presence/absence data.
}
  \item{\dots}{
Additional arguments passed to \code{vegan::vegdist}, except argument \code{method} which is supplied by the \pkg{vegsoup} object.
}
}
\details{
If the resulting \code{"dist"} object is not a dissimilarity matrix (\code{max > 1}), it will be divided by \code{max(D)} to obtain dissimilarities. Note, the method returned by \code{decostand(obj)} will be applied before calculation of the distance matrix. Use \code{vegdist(obj)} to query which method is set for an object. Note, also the \code{print} and \code{show} methods will report this value. 

}
\value{
Returns an object of class \code{"dist"}.
}
\author{
Roland Kaiser
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vegsoup:vegdist]{vegdist}}
}
\keyword{methods}

