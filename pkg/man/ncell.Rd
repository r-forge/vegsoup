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

\title{Number or rows, columns, and cells of a VegsoupData* object}   

\description{
Get the number of rows, columns, or cells of the species matrix contained in a VegsoupData* object.  
}

\usage{
ncol(x)
nrow(x)
dim(x)
ncell(x)
%% ncol(x) <- value
%% nrow(x) <- value
}

\details{
\code{nrow(x)} allways equal the number of rows in the sites slot. \code{ncol(x)} gives the number of species including layer dulicates. For species richness see \code{\link{Richness}}. \code{dim} retrieves the dimension of the species matrix of an object and \code{ncell} the number of cells in that matrix.
}

\arguments{
\item{x}{ a VegsoupData* object }
}

\value{
An integer giving number of rows (plots) and species (each occurrence of a species in layer), or the dimension of that matrix, respectively the product of this two-dimensional matrix.
}

\seealso{
\code{\link{VegsoupData}, \code{\link{Richness}}
}
}
\author{
Roland Kaiser
}

\examples{
	data(testdata)
	dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))
	dim(dta)
	all.equal(prod(dim(dta)), ncell(dta))	  
}	
