\encoding{UTF-8}

\name{decostand}
\docType{methods}

\alias{decostand}
\alias{decostand,Vegsoup-method}

\alias{decostand<-}
\alias{decostand<-,Vegsoup-method}
\alias{decostand<-,VegsoupPartition-method}
\alias{decostand<-,Vegsoup,character-method}
\alias{decostand<-,VegsoupPartition,character-method}
\alias{decostand<-,Vegsoup,NULL-method}
\alias{decostand<-,VegsoupPartition,NULL-method}

\title{ Standardization Methods for \code{Vegsoup*} objects.
}

\description{
This method provides the standardization methods implemented in function \code{\link[vegan:decostand]{decostand}} of \pkg{vegan} plus one more.  
Only the species matrix will be affected by standardization.  
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
  Standardization is often necessary before the species matrix is subject to analysis by a multivariate
  method. This can easily be achieved by setting \emph{slot 'decostand'} using the method of the same name.
  The standardization method will be applied when any method for that object requests a species matrix
  (see \code{\link{as.matrix}}). It is possible to supply more than one method and these are applied
  sequentially to the data (order matters). In fact, method \code{wisconsion} invokes a double standardization
  (\code{max} followed by \code{total}) as it is the case in \pkg{vegan}. Currently only the
  \code{method} argument of \code{vegan::decostand} is implemented. Whereas argument
  \code{MARGIN} is not.

  Method \code{cap} calculates cumulative abundance profiles (CAP, De \enc{Cáceres}{Caceres} et al. 2013).
  Currently any additional decostand method will be ignored. This method is especially suited for
  forest vegetation when vertical stand structure is available (vegetation layers).
  
  If the decostand method is redefined for an object of class \code{'VegsoupPartion'} the
  object will be subject to revaluation as to honor the newly assigned decostand method. In this way
  it is possible to compare different standardizations with the same partitioning method.
}

\value{
\code{decostand(x)} returns a character vector depending on the number of methods.

\code{decostand(x) <- value} sets the standardization method(s).
}
\references{
De \enc{Cáceres}{Caceres}, M., Legendre, P., He, F. 2013 Dissimilarity measurements and the size structure of ecological communities. \emph{Methods in Ecology and Evolution} \bold{4}, 1167--1177
}
\author{ Roland Kaiser }

\note{
  Manipulation of the sites data has to be performed by modifying elements of
  \emph{slot 'sites'}, which is represented as \code{data.frame}. Use the extract and replace
  operators \code{\link[=Extract]{"$"}} and
  \code{\link{names<-}} method to modify columns of the sites data. See \code{\link[vegsoup]{Extract}}
  for details.
}

\seealso{
\code{\link{as.matrix}},
\code{\link{as.dist}},
\code{\link{vegdist}}
\code{\link{coverscale}}
}
\examples{
data(barmstein)
x <- barmstein
	
# no standardization by default
decostand(x)

decostand(x) <- "total"

# retrieve a strandardized species matrix
range(as.numeric(x))

# double standardization
decostand(x) <- c("hellinger", "standardize")
range(as.numeric(x))

# remove any standardization
decostand(x) <- NULL

# replace decostand method for VegsoupPartition object
x <- barmstein
decostand(x) <- "pa"

x <- VegsoupPartition(x, k = 2)
decostand(x) <- NULL

x <- barmstein
decostand(x) <- "max"

x <- VegsoupPartition(x, k = 2)
decostand(x)
decostand(x) <- "wisconsin"

# cumulative abundance profiles (CAP)
data(windsfeld)
x <- windsfeld

#	subset forest plots
x <- x[x$tcov1 != 0 | x$tcov2 != 0, ]

#	order layers
layers(x)
layers(x) <- layers(x)[c(2,1,3,5,4)]

#	select woody species
j <- c("lari.deci", "pinu.mugo", "sorb.aucu",
	"loni.caer.caer", "sali.appe", "pice.abie")
j <- unique(unlist(sapply(j, grep, colnames(x))))
x <- x[, j]

taxon(x)

#	raw matrix
m1 <- as.numeric(x)

# cumulative abundance profiles
decostand(x) <- c("cap")

m2 <- as.numeric(x)

m1[,grep("lari.deci", colnames(m1)), drop = FALSE]
m2[,grep("lari.deci", colnames(m2)), drop = FALSE]
}

\keyword{methods}

