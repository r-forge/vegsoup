\name{Subset} 

\docType{methods} 

\alias{Subset}
\alias{[-methods}
\alias{[,VegsoupData-method}
\alias{[,VegsoupDataPartition-method}

\title{Subsetting Objects}

\description{
Method to subset an object by indexing the species matrix.
}

\usage{
\S4method{aggregate}{Raster}(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename='', ...)
}

\arguments{
  \item{x}{VegsoupData* object}    
}

\details{
Argument \code{drop} is internaly set to \code{TRUE}. This ensures that no dimensions will be dropped a matrix is returned (see \code{\link{drop}}). 
}
\section{Methods}{
\describe{
This method allways subset all slot of the object. The order implied by the indices are retained in the objects internal order.
}
}

\seealso{ \code{\link{rbind}, \code{\link{VegsoupData}} and \code{\link{as.matrix}}}}


\author{Roland Kaiser}

\examples{
	qry <- Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet"))
	dta <- VegsoupData(qry)
	
	s1 <- dta[1:2, ]
	s2 <- dta[3:4, ]
	s3 <- dta[5:6, ]

#	rbind
	res <- rbind(s3, s1, s2)
	
}
\keyword{methods}

