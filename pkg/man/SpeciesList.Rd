\name{SpeciesList}
\alias{SpeciesList}
\title{
Return Species List from an Object Extending Class Vegsoup 
}
\description{
Simpliy return a list of taxon names and corresponding unique key \code{"abbr"}. 
}
\usage{
SpeciesList(obj, layered)
}
\arguments{
  \item{obj}{
An object inheriting from class \code{Vegsoup}.
}
  \item{layered}{
return species list with layers, defaults to \code{FALSE}. See \sQuote{Details}.
}
}
\details{
If \code{layered = TRUE} also return all layers where a species occurs.
}
\value{
A \code{data.frame} with two columns if \code{layered = FALSE}, or three columns if \code{layered = FALSE}. The first giving the unique taxon key (\code{"abbr"}), the second the full taxon name \code{"taxon"}, and the last the layer (\code{"layer"}) where the taxon occurs in.
}
\note{
The method will not print anything to console.	
}
\author{
Roland Kaiser
}
\examples{
library(vegsoup)
data(testdata)

qry <- Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet"))	

sl <- SpeciesList(qry)
head(sl)

sl <- SpeciesList(qry, layered = TRUE)
head(sl)

}
\keyword{methods}


