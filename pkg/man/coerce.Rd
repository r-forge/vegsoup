\name{coerce-methods}
\docType{methods}
\alias{coerce-methods}
\alias{coerce,matrix-method}
\alias{coerce.array-method}
\alias{coerce,sparseMatrix-method}
\alias{coerce,dist-method}
\alias{coerce,list-method}
\alias{coerce.data.frame-method}
\alias{coerce,data.list-method}

\title{ Coerce Methods for \sQuote{Vegsoup*} objects  }
\description{
Coerce object a \code{'Vegsoup*'} object to other class.
}
\usage{
## S4 method for signature from = 'Vegsoup*' to = 'matrix'
as(x, "matrix")

## S4 method for signature from = 'Vegsoup*' to = 'array'
as(x, "array")

## S4 method for signature from = 'Vegsoup*' to = 'dist'
as(x, "dist")

## S4 method for signature from = 'Vegsoup*' to = 'sparseMatrix'
as(x, "sparseMatrix")
as(x, "dsparseMatrix")

## S4 method for signature from = 'Vegsoup*' to = 'list'
as(x, "list")

## S4 method for signature from = 'Vegsoup*' to = 'data.frame'
as(x, "data.frame")

## S4 method for signature from = 'Vegsoup*' to = 'data.list'
as(x, "data.list")
}
\arguments{
  \item{ x }{
    A Vegsoup* object
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

  Coercion to sparse matrices is currently very limited.
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
there are in \code{Species(x)}.

Coercion to 'array' returns an \code{"array"}.

}
\section{ Warning }{
  S3 functions must contain a call to \code{as.matrix} to gurantee dispatch.
  For example \code{metaMDS(obj)} does not work, but other functions in
  package vegan will. Note, if a function that expects a species matrix calls
  \code{as.data.frame} in the function body it will certainly break, because
  the \code{as.data.frame(x)} coercion does not return a species matrix (see
  \code{\link{as.data.frame}})!
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{as.matrix}},
\code{\link{as.dist}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# same as as.matrix(prt, typeof = "numeric")
x <- as(prt, "matrix")

# the species matrix as an multi-dimensional array
x <- as(dta, "array")

# the data stored in slots 'species', 'sites' and 'taxonomy'
x <- as(dta, "list")
dim(x)

# coerce to data.list in package multitable
x <- as(dta, "data.list")
summary(x)
class(as.data.list(dta))

# dispatch: use Vegsoup* object in S3 function
decostand(dta) <- "hellinger"
vegdist(dta) <- "euclidean"

rda(dta) # principal component analysis
agnes(dta) # agglomerative nesting

# no call to as.dist(d) in hclust
hclust(as.dist(dta))
}
\keyword{ methods }
