\name{vegdist}
\docType{methods}
\alias{as.dist}
\alias{as.dist-methods}
\alias{vegdist}
\alias{vegdist-methods}
\alias{vegdist<--methods}
\alias{distconnected}
\alias{distconnected-methods}
\alias{distconnected,VegsoupData-method}
\alias{distconnected,VegsoupDataPartition-method}

\title{ Distance Matrices from \code{Vegsoup*} Objects }
\description{
Compute a distance matrix as specified by \code{vegdist(obj)} by internal calls to \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.  
}
\usage{
as.dist(obj)

vegdist(obj, ...)
vegdist(obj) <- value

distconnected(obj, ...)
}
\arguments{
  \item{ obj }{
    An object of class \code{Vegsoup*}.
  }
  \item{ binary }{
    Compute the distance matrix based on presence/absence data.
  }
%% ... setGeneric("as.dist") will break dispatch for typal
%% see code in as.dist.R
%%\item{ mode }{
%%  Compute distances in \emph{Q} or \emph{R} mode. See
%%  \code{\link{as.matrix}}.
%%}
  \item{ value }{
    A single character valid as argument \code{method} to function
    \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.
  }
  \item{ \dots }{
    For \code{vegdist} additional arguments passed to \code{vegan::vegdist},
    except argument \code{method} which is inherited supplied by the
    \code{Vegsoup*} object.
    For \code{distconnected}
  }
}
\details{
%%If the resulting \code{"dist"} object is not a dissimilarity matrix
%%(\code{max > 1}), it will be divided by \code{max(D)} to obtain
%%dissimilarities.

  Note, the method returned by \code{decostand(obj)} will be applied before
  calculation of the distance matrix. Use \code{vegdist(obj)} to query which
  method is active for an object. The \code{print}, \code{show} and
  \code{summary} methods will report this value of \code{Vegsoup*} objects.
  The argument \code{mode} controls if the distances are obtained for objects,
  the plots (\emph{Q mode}) or variables, the species (\emph{R mode}).

  Method \code{distconnected} is a simple wrapper for function
  \code{\link[vegan:distconnected]{distconnected}} in package
  \pkg{vegan} and checks for connectedness of dissimilarities.
}
\value{
Returns an object of class \code{"dist"} with an additional attribute \code{"mode"} one of \code{"Q"} or \code{"R"}.
}
\author{
Roland Kaiser
}
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
\keyword{ methods }
