\name{vegdist}
\docType{methods}
\alias{as.dist}
\alias{as.dist-methods}
\alias{vegdist}
\alias{vegdist-methods}
\alias{vegdist<--methods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a Distance Matrix from Objects of Class Vegsoup*
}
\description{
Compute a distance matrix as specified by \code{vegdist(obj)} by internal calls to \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.  
}
\usage{
as.dist(obj, mode)

vegdist(obj, ...)
vegdist(obj) <- value
}
\arguments{
  \item{ obj }{
    An object of class \code{Vegsoup*}.
  }
  \item{ binary }{
    Compute the distance matrix based on presence/absence data.
  }
  \item{ mode }{
    Compute distances in \emph{Q} or \emph{R} mode. See
    \code{\link{as.matrix}}.
  }
  \item{ value }{
    A single character valid as argument \code{method} to function
    \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.
  }
  \item{ \dots }{
    Additional arguments passed to \code{vegan::vegdist}, except argument
    \code{method} which is supplied by the \pkg{vegsoup} object.
  }
}
\details{
If the resulting \code{"dist"} object is not a dissimilarity matrix (\code{max > 1}), it will be divided by \code{max(D)} to obtain dissimilarities. Note, the method returned by \code{decostand(obj)} will be applied before calculation of the distance matrix. Use \code{vegdist(obj)} to query which method is active for an object. Note, also the \code{print} and \code{show} methods will report this value. The argument \code{mode} controls if the distances are obtained for objects, the plots (\emph{Q mode}) or variables, the species (\emph{R mode}).
}
\value{
Returns an object of class \code{"dist"} with an additional attribute \code{"mode"} one of \code{"Q"} or \code{"R"}.
}
\author{
Roland Kaiser
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vegan:vegdist]{vegdist}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# query disssimlilarity measure set for the object
# change the default distance 'euclidean'
vegdist(dta) 

vegdist(dta) <- "bray"

d.q <- as.dist(dta, "numeric")
class(d.q)
attributes(d.q)
attr(d.q, "mode")

# R-mode analysis
# chi square distances can be obtained
# by computing euclidean distances on standardized data
# see ?vegan::vegdist 
decostand(dta)  <- "chi.square"

d.r <- as.dist(dta, mode = "R")              
class(d.r)
attr(d.r, "mode")   
}
\keyword{methods}

