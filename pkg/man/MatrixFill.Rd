\name{MatrixFill}
\alias{MatrixFill}

\title{
Computes Matrix Fill of Object Extending VegsoupData
}
\description{
Calculates the the proportion (percentage) of all non zero elements in a species matrix.
}
\usage{
MatrixFill(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{VegsoupData}
}
}
\value{
simply returns a vector of length one and mode \code{numeric} 

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{nestedtemp}} in \pkg{vegan} for nestedess indices
}
\examples{
library(vegsoup)

data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
MatrixFill(dta)

#	use object of class VegsoupData in vegan
nestedbetajac(as.binary(dta))
}
\keyword{methods}

