\name{coerce}
\docType{methods}

\alias{as,Vegsoup-method}

\alias{coerce}
\alias{coerce,Vegsoup,matrix-method}
\alias{coerce,Vegsoup,array-method}
\alias{coerce,Vegsoup,sparseMatrix-method}
\alias{coerce,Vegsoup,dsparseMatrix-method}
\alias{coerce,Vegsoup,dist-method}
\alias{coerce,Vegsoup,list-method}
\alias{coerce,Vegsoup,data.frame-method}
\alias{coerce,Vegsoup,data.list-method}
\alias{coerce,Vegsoup,mefa-method}

\title{ Coerce Methods for \sQuote{Vegsoup*} objects  }

\description{
Coerce object a \code{'Vegsoup*'} object to other class.
}

\usage{
\S4method{coerce}{Vegsoup,matrix}(from,to)
\S4method{coerce}{Vegsoup,array}(from,to)
\S4method{coerce}{Vegsoup,dist}(from,to)
\S4method{coerce}{Vegsoup,sparseMatrix}(from,to)
\S4method{coerce}{Vegsoup,dsparseMatrix}(from,to)
\S4method{coerce}{Vegsoup,list}(from,to)
\S4method{coerce}{Vegsoup,data.frame}(from,to)
\S4method{coerce}{Vegsoup,data.list}(from,to)
\S4method{coerce}{Vegsoup,mefa}(from,to)
}

\arguments{
  \item{ from }{
    A Vegsoup* object.
  }
  \item{ to }{
    class to coerce to.
  }
}

\details{
  These are generic coerce methods to ensure that S3 functions dispatch
  properly, especially regarding the \code{as(x, "matrix")} and \code{as(x,
  "dist")} coercion methods. In this way it is almost allways possible to
  supply a \code{'Vegsoup*'} object directly to functions for multivariate
  analysis (see \sQuote{Examples}). Note, the matrix coercion retrieves a
  species matrix according to the defaults of \code{\link{as.matrix}} and the
  list coercion calls \code{as.matrix(x, typeof = "character", mode = "Q")}.

  Coercion to sparse matrices is currently rather limited.
}

\value{
  Coercion to \code{'matrix'} and \code{'sparseMatrix'} returns an object of the corresponding class.
  Coercion to 'list' returns a list with three components.
  \item{ species }{
     object of class \code{'matrix'}
  }
  \item{ sites }{
     object of class \code{'data.frame'}
  }
  \item{ taxonomy }{
    object of class \code{'data.frame'}
  }

  Coercion to 'data.frame' returns a \code{"data.frame"} with as many row as
  there are in \code{species(x)}.

  Coercion to 'array' returns an \code{"array"}.

  Coercion to 'mefa' and 'data.list' returns a \code{"mefa"} respectively
  \code{"data.list"} object.

}

\section{ Warning }{
  S3 functions must contain a call to \code{as.matrix} to gurantee dispatch.
  For example \code{metaMDS(obj)} does not work, but other functions in
  package vegan will. Note, if a function that expects a species matrix calls
  \code{as.data.frame} in the function body it will certainly break, because
  the \code{as.data.frame(x)} coercion does not return a species matrix (see
  \code{\link{as.data.frame}})!

  Coercion to class \code{"mefa"} is only meaningful for count data!
}

\author{ Roland Kaiser }

\seealso{
\code{\link{as.matrix}}, \code{\link{as.dist}}
}

\examples{
require(vegsoup)
require(cluster)

data(barmstein)
x <- barmstein

# same as as.matrix(prt, typeof = "numeric")
m <- as(x, "matrix")

# the species matrix as an multi-dimensional array
a <- as(x, "array")

# the data stored in slots 'species', 'sites' and 'taxonomy'
l <- as(x, "list")
length(l)

# coerce to data.list in package multitable
xdl <- as(x, "data.list")
summary(xdl)

# automatic dispatch of Vegsoup* object as argument in S3 function
# if they internally call as.matrix or as.dist
decostand(x) <- "hellinger"
vegdist(x) <- "euclidean"

rda(x) # principal component analysis

agnes(x) # agglomerative nesting

# no call to as.dist(d) in hclust, no automatic dispatch
hclust(as.dist(x))
}

\keyword{ methods }

