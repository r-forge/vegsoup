\name{decostand}
\docType{methods}

\alias{decostand}
\alias{decostand,Vegsoup-method}

\alias{decostand<-}
\alias{decostand<-,Vegsoup-method}
\alias{decostand<-,Vegsoup,character-method}
\alias{decostand<-,Vegsoup,NULL-method}

\title{ Standardization Methods for \code{Vegsoup*} objects.
}

\description{
This wrapper function provides the standardization methods implemented in function \code{\link[vegan:decostand]{decostand}} of \pkg{vegan}. Only the species matrix will be affected by standardization. 
}

\usage{
\S4method{decostand}{Vegsoup}(x)

\S4method{decostand}{Vegsoup}(x) <- value
}

\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
  \item{ value }{
    A vector of characters giving methods as accepted by
    \code{vegan::decostand} or \code{NULL}.
  }
}

\details{
Standardization is often necessary before the species matrix is subject to analysis by a multivariate method. This can easily be achieved by setting the \code{'decostand'} slot of the object. The standardization method will be applied when a method for that object requests a species matrix. It is possible to supply more than one method and these are sequentially to the data (order matters). In fact, method \code{'wisconsion'} invokes a double standardization (\code{'max'} followed by \code{'total'}).

Currently only the \code{'method'} argument of \code{vegan::decostand} is implemented. Whereas argument \code{MARGIN} is not.
}

\value{
\code{decostand(obj)} returns a character vector depending on the number of methods.

\code{decostand(obj) <- value} sets the standardization method(s).
}

\author{ Roland Kaiser }

\note{
Manipulation of the sites data has to be performed by modifying elements of the \code{'sites'} slot, which is represented as \code{data.frame}.
Use the extract and replace operators \code{\link[=Extract]{"$"}} and \code{\link{names<-}} method to modify columns of the sites data. See \code{\link[vegsoup:Extract]{Extract}} for details.
}

\seealso{
\code{\link{as.matrix}},
\code{\link{as.dist}},
\code{\link{vegdist}}
\code{\link{coverscale}}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein
    
# no standardization by default
decostand(x)

decostand(x) <- "total"

# retreive a strandardized species matrix
as.numeric(x)

# double strandardization
decostand(x) <- c("hellinger", "standardize")
as.numeric(x)
    
# remove any standardization
decostand(x) <- NULL

}

\keyword{methods}

