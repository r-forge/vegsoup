\name{ncell}
\docType{methods}
\alias{ncol}
\alias{ncol-methods}
\alias{nrow}
\alias{nrow-methods}
\alias{dim}
\alias{dim-methods}
\alias{ncell}
\alias{ncell-methods}

\title{Number or Rows, Columns, and Cells of a \code{Vegsoup*} Object }

\description{
  Get the number of rows, columns, or cells of the species matrix contained in
  a Vegsoup* object.
}

\usage{
  ncol(x)

  nrow(x)

  dim(x)

  ncell(x)
}
\arguments{
  \item{ x }{
    A \code{Vegsoup*} object
  }
}

\details{
\code{nrow(x)} allways equal the number of rows in the sites slot. \code{ncol(x)} gives the number of species including layer dulicates. For species richness see \code{\link{Richness}}. \code{dim} retrieves the dimension of the species matrix of an object and \code{ncell} the number of cells in that matrix.
}

\value{
An integer giving number of rows (plots) and species (each occurrence of a species in layer), or the dimension of that matrix, respectively the product of this two-dimensional matrix.
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
ncell(dta)
}   
