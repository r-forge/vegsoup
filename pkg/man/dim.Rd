\encoding{UTF-8}

\name{dim}
\docType{methods}

\alias{nrow}
\alias{nrow,Vegsoup-method}

\alias{ncol}
\alias{ncol,Vegsoup-method}

\alias{ncol}
\alias{nrow,Vegsoup-method}

\alias{dim}
\alias{dim,Vegsoup-method}

\alias{ncell}
\alias{ncell,Vegsoup-method}

\title{ Number or Rows, Columns, and Cells }   

\description{
Get the number of rows, columns, or cells of the species matrix contained in a Vegsoup* object.
}

\usage{
\S4method{ncol}{Vegsoup}(x)

\S4method{nrow}{Vegsoup}(x)

\S4method{dim}{Vegsoup}(x)

\S4method{ncell}{Vegsoup}(x)

\S4method{nrow}{Vegsoup}(x)

## not yet implemented, subsample to disred dimension
## ncol(x) <- value
## nrow(x) <- value
## ncell(x) <- value
}

\details{
  \code{nrow(x)} allways equals the number of plots. \code{ncol(x)} gives the number of species
  including layer duplicates. For species richness see \code{\link{richness}}. \code{dim}
  retrieves the dimension of the species matrix of an object and \code{ncell} the numbder of
  cells in the species matrix.
}

\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
}

\value{
  An integer giving number of rows (plots) and species (each occurrence of a
  species in layer), or the dimension of that matrix, respectively the product
  of this two-dimensional matrix.
}

\seealso{
\code{\link{Vegsoup}},
\code{\link{richness}}
}

\author{ Roland Kaiser }

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

dim(x)
nrow(x)  # number of plots
ncol(x)  # number of (pseudo-)species
ncell(x) # number of cells, short hand for prod(dim(dta))
}

\keyword{methods}

