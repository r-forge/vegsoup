\encoding{UTF-8}

\name{Species}
\docType{methods} 


\alias{species,Vegsoup-method}

\alias{species<-}
\alias{species<-,Vegsoup-method}
\alias{species<-,Vegsoup,Species-method}
\alias{species<-,Vegsoup,SpeciesTaxonomy-method}
\alias{species<-,Vegsoup,data.frame-method}

\title{ Methods to Retrieve or Manipulate the Species Slot of \code{Vegsoup*} Objects }
\description{
Accessor method to retrieve the species data in \emph{long format} as stored as class \code{}inside the object.
}
\usage{
\S4method{species}{Vegsoup}(obj)
\S4method{species}{Vegsoup,Species}(obj) <- value
\S4method{species}{Vegsoup,SpeciesTaxonomy}(obj) <- value
\S4method{species}{Vegsoup,data.frame}(obj) <- value
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ value }{
    Not yet implemented.
  }  
}
\details{
Merely a set of accessor and replacement methods for \emph{slot Species} of \code{"Vegsoup"} objects. The \code{"species<-"} method for value class \code{"Species"} can be used to take a subset based on \emph{slot Species} (see \sQuote{Examples}). Note, this method works only for subsetting an object and you can't add new species observations by this method. The \code{"SpeciesTaxonomy"} replacement is needed in such circumstances. In either case, the \code{value} object can't add any plots that not already exist (those returned by \code{rownames(obj)}). However, changes in the taxonomy component (an addition of new species) can be achieved using this replacement method. The \code{"data.frame"} exists for completeness, but is currently not implemented.
}
\value{
\code{"Species"} object or a subset of the input object depending on value.
}

\author{ Roland Kaiser }

\seealso{
\code{\linkS4class{Vegsoup}}
}

\examples{
require(vegsoup)

data(barmstein)
x <-  barmstein

# retrieve data.frame from an Vegsoup object
spc <- species(x)
class(spc)
barplot(table(spc$cov))

# this is the number of all species observations over all plots and strata
n <- nrow(species(species(x)))

# a random sample
i <- sort(sample(n, 10))

# note, we (might) lose a plot, for sure we only get a subset of species
richness(x)
species(x)[i, ]

# now assign to Vegsoup obejct
species(x) <- species(x)[i, ]
richness(x)
}
\keyword{ methods }

