\name{vegdist}
\docType{methods}
\alias{as.dist}
\alias{as.dist-methods}
\alias{vegdist}
\alias{vegdist-methods}
\alias{vegdist<--methods}
\alias{vegdist<-,Vegsoup,character-method}
\alias{distconnected}
\alias{distconnected-methods}

\title{ Distance Matrices from \code{Vegsoup*} Objects }
\description{
Compute a distance matrix as specified by \code{vegdist(obj)} by internal calls to \code{\link[vegan:vegdist]{vegdist}} of \pkg{vegan}. Method \code{distconnected} is wrapper for function \code{\link[vegan:distconnected]{distconnected}} also found in package vegan.
}
\usage{
as.dist(m, diag = FALSE, upper = FALSE)

vegdist(x, method = "bray", binary = FALSE, diag = FALSE,
	upper = FALSE, na.rm = FALSE, ...)
vegdist(x) <- value

distconnected(dis, toolong, trace)
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
  %% If the resulting \code{"dist"} object is not a dissimilarity matrix
  %% (\code{max > 1}), it will be divided by \code{max(D)} to obtain
  %% dissimilarities.

  Note, the method returned by \code{decostand(obj)} will be applied before
  calculation of the distance matrix. Use \code{vegdist(obj)} to query which
  method is active for an object. The \code{print}, \code{show} and
  \code{summary} methods will report this value for a \code{Vegsoup*} object.
  %%  The argument \code{mode} controls if the distances are obtained for objects,
  %%  the plots (\emph{Q mode}) or variables, the species (\emph{R mode}).

  Method \code{distconnected} is a simple wrapper for function
  \code{\link[vegan:distconnected]{distconnected}} in package
  \pkg{vegan} and checks for connectedness of dissimilarities.
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
# currently disfunct due to missing ... argument in stats:as.dist
# chi square distances can be obtained
# by computing euclidean distances on standardized data
# see ?vegan::vegdist 
##decostand(dta)  <- "chi.square"
##d.r <- as.dist(dta, mode = "R")              
##class(d.r)
##attr(d.r, "mode")

distconnected(dta)
}
\keyword{ methods }
