\encoding{UTF-8}

\name{vegdist}
\docType{methods}

\alias{as.dist}
\alias{as.dist,Vegsoup-method}

\alias{vegdist}
\alias{vegdist,Vegsoup-method}

\alias{vegdist<-}
\alias{vegdist<-,Vegsoup-method}
\alias{vegdist<-,VegsoupPartition-method}
\alias{vegdist<-,Vegsoup,character-method}
\alias{vegdist<-,VegsoupPartition,character-method}

\alias{distconnected}
\alias{distconnected,Vegsoup-method}

\title{ Dissimilarity Matrices from \code{Vegsoup*} Objects }
\description{
Compute a dissimilarity matrix as specified by \code{vegdist(x)} by internal calls to \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}. Method \code{distconnected} is a wrapper for function \code{\link[vegan:distconnected]{distconnected}} also found in package vegan.
}

\usage{
\S4method{as.dist}{Vegsoup}(m, diag = FALSE, upper = FALSE)

\S4method{vegdist}{Vegsoup}(x, method = "bray", binary = FALSE, diag = FALSE,
	upper = FALSE, na.rm = FALSE, ...)

\S4method{vegdist}{Vegsoup}(x) <- value

\S4method{distconnected}{Vegsoup}(dis, toolong, trace)
}

\arguments{
  \item{ x, m, dis }{
	\code{Vegsoup*} object.
  }
  %% \item{ mode }{
  %%   Compute distances in \emph{Q} or \emph{R} mode. See
  %%   \code{\link{as.matrix}}.
  %% }
  \item{ value }{
	A single character valid as argument \code{method} to function
	\code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}.
  }
  \item{ diag, upper, na.rm }{
	Not used.
  }
  \item{ method, binary }{
	Not used.
  }
  \item{ \dots }{
	For \code{vegdist} additional arguments passed to \code{vegan::vegdist},
	except argument \code{method} which is inherited from the \code{Vegsoup*} object.
	For \code{distconnected} arguments passed to \code{\link[vegan:vegdist]{vegdist}}.
  }
  \item{ toolong }{
	Shortest dissimilarity regarded as NA. See \code{\link[vegan:distconnected]{vegdist}}.
  }
  \item{ trace }{
	Summarize results.
  }
}
\details{
  Note, the method returned by \code{decostand(obj)} will be applied before
  calculation of the distance matrix. Use \code{vegdist(obj)} to query which
  method is active for an object. The \code{print}, \code{show} and
  \code{summary} methods will report this value for a \code{Vegsoup*} object.
  
  If \code{vegdist} is rested using the replacement method \code{vegdist<-} for
  objects of classes \code{'VegsoupPartition'} and \code{'VegsoupPartitionFidelity'}
  the respective objects are recomputed as to honor the changed distance method.
  %%  The argument \code{mode} controls if the distances are obtained for objects,
  %%  the plots (\emph{Q mode}) or variables, the species (\emph{R mode}).

  Method \code{distconnected} is a simple wrapper for function
  \code{\link[vegan:distconnected]{distconnected}} in package
  \pkg{vegan} and checks for connectedness of dissimilarities.
}
\note{
  As \code{vegan::vegdist}, \code{as.dist} returns dissimilarities not distances.
  If the resulting \code{"dist"} object is not a dissimilarity matrix
  (\code{max(D) > 1}), it will be divided by \code{max(D)} to obtain
  dissimilarities.
}
\value{
\code{as.dist} returns an object of class \code{"dist"} with an additional attribute \code{"mode"}. % one of \code{"Q"} or \code{"R"}.

\code{vegdist} returns a \code{"character"}, the distance index active for a \code{Vegsoup*} object.

\code{distconnected} returns an \code{"integer"} vector to identify connected groups.
}
\author{
Roland Kaiser

\code{vegdist} and \code{distconnected} are methods for functions in package \pkg{vegan}.
}
\seealso{
\code{\link[vegan:vegdist]{vegdist}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# query disssimlilarity measure set for the object
vegdist(x)

# change the default distance 'euclidean'
vegdist(x) <- "bray"

d <- as.dist(x, "numeric")
class(d)
attributes(d)
attr(d, "mode")

# replace vegdist method for VegsoupPartition object
x <- VegsoupPartition(barmstein, k = 2)
vegdist(x)
p1 <- partitioning(x)

# change vegdist
vegdist(x) <- "bray"
p2 <- partitioning(x)

table(p1, p2)

# R-mode analysis
# currently disfunct due to missing ... argument in stats:as.dist
# chi square distances can be obtained
# by computing euclidean distances on standardized data
# see ?vegan::vegdist

\dontrun{ 
# decostand(x)  <- "chi.square"
# d <- as.dist(dta, mode = "R")
# class(d)
# attr(d, "mode")
}

# connectivity of distances
distconnected(x)
}
\keyword{ methods }

