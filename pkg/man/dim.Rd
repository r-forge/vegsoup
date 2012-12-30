\name{dim}

\docType{methods}

\alias{ncol}
\alias{ncol-methods}
\alias{ncol,VegsoupData-method}
\alias{ncol}
\alias{nrow-methods}
\alias{nrow,VegsoupData-method}
\alias{dim}
\alias{dim-methods}
\alias{dim,VegsoupData-method}

\title{Number or rows, columns, and cells of a VegsoupData* object}   

\description{
Get the number of rows, columns, or cells of the species matrix contained in a VegsoupData* object.  
}

\usage{
ncol(x)
nrow(x)
dim(x)

%% ncell(x)
%% ncol(x) <- value
%% nrow(x) <- value
}

\details{
\code{nrow(x)} allways equal the number of rows in the sites slot. \code{ncol(x)} gives the number of species including layer dulicates. For species richness see \code{\link{Richness}}. \code{dim} retrieves the dimension of the species matrix of an object.
}

\arguments{
\item{x}{a VegsoupData object}
}

\value{
An integer giving number of rows (plots) and species (each occurrence of a species in layer).
}

\seealso{ \code{\link{VegsoupData}}, \code{\link{Richness}} }

\author{ Roland Kaiser }

\examples{
	data(testdata)
	dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))
	dim(dta)	  
}	
\keyword{methods}

