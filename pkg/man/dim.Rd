\name{dim}
\docType{methods}
\alias{ncol}
\alias{ncol-methods}
\alias{ncol}
\alias{nrow-methods}
\alias{dim}
\alias{dim-methods}
\alias{ncell}
\alias{ncell-methods}

\title{Number or Rows, Columns, and Cells of a Vegsoup* object}   

\description{
Get the number of rows, columns, or cells of the species matrix contained in a Vegsoup* object.
}

\usage{
  ## S4 method for signature 'Vegsoup*'
  ncol(x)
  ## S4 method for signature 'Vegsoup*'
  nrow(x)
  ## S4 method for signature 'Vegsoup*'
  dim(x)
  ## S4 method for signature 'Vegsoup*'
  ncell(x)

  ## not yet implemented
  ## ncol(x) <- value
  ## nrow(x) <- value
  ## ncell(x) <- value
}
\details{
\code{nrow(x)} allways equals the number of rows in the sites slot. \code{ncol(x)} gives the number of species including layer duplicates. For species richness see \code{\link{Richness}}. \code{dim} retrieves the dimension of the species matrix of an object and \code{ncell} the numbder of cells in the species matrix.
}
\arguments{
  \item{ x }{
    A Vegsoup* object
  }
}

\value{
  An integer giving number of rows (plots) and species (each occurrence of a
  species in layer), or the dimension of that matrix, respectively the product
  of this two-dimensional matrix.
}
\seealso{
\code{\link{Vegsoup}},
\code{\link{Richness}}
}

\author{ Roland Kaiser }

\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

dim(dta)
nrow(dta)  # number of plots
ncol(dta)  # number of (pseudo-)species
ncell(dta) # number of cells, short hand for prod(dim(dta))
}
\keyword{methods}
