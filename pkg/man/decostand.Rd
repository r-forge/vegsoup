\name{decostand}
\alias{decostand}
\alias{decostand<-}
\alias{decostand-methods}
\alias{decostand<--methods}  

\title{
Standardization Methods For Objects inheriting Class VegsoupData.
}
\description{
This wrapper function provides the standardization methods implemented in function \code{\link[vegan:decostand]{decostand}} of \pkg{vegan}. Only the species matrix will be affected by standardization. 
}
\usage{
decostand(obj)

decostand(obj) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
	An object inheriting from class \code{VegsoupData}.
}
  \item{value}{
	a vector of characters giving methods as accepted by  \code{vegan::decostand} or \code{NULL}.
}
}
\details{
Standardization is often nessecary before the species matrix is subject to analysis by a multivariate method. This can easyly be achieved by setting the \code{'decostand'} slot of the object. The standardization method will be applied when a method for that object requests a species matrix. It is possible to supply more than one method and these are applied in sequence to the data (order matters). In fact, method \code{'wisconsion'} invokes a dauble standardization (\code{'max'} followed by \code{'total'}).

Currently only the \code{'method'} argument of \code{vegan::decostand} is implemented. Whereas argument \code{MARGIN} is not.
}
\value{
\code{decostand(obj)} returns a character vector depending on the number of methods.

\code{decostand(obj) <- value} sets the standardization method(s).
}

\author{
Roland Kaiser
}
\note{
Manipulation of the sites has to be done by modifing elements of the \code{'sites'} slot, which is represented as \code{data.frame}.
Use the extract and replace operators \code{\link[=Extract]{"$"}} to midify colums of the sites data. See \code{\link[vegsoup:Extract]{Extract}} for details.
}

\seealso{
\code{\link{as.numeric}} and \code{\link{as.binary}}
}
\examples{
library(vegsoup)
data(testdata)        
qry <- Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet"))
dta <- VegsoupData(qry)
	
# no standardization by default
decostand(dta)
	
# create object of class VegsoupData
# and supply standardization in one step
dta <- VegsoupData(qry, decostand = "hellinger")
decostand(dta)  
	
# retreive the strandardized species matrix
as.numeric(dta)

# double strandardization
decostand(dta) <- c("hellinger", "standardize")
as.numeric(dta)
	
# remove any standardization
decostand(dta) <- NULL 
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

