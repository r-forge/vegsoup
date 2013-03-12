\name{Species}
\docType{methods} 
\alias{Species}
\alias{Species-methods}
\alias{Species<-}
\alias{Species<--methods}

\title{ Methods to Retrieve or Manipulate the Species Slot of \code{Vegsoup*} Objects }
\description{
Accessor method to retrieve the species data in \emph{long format} as stored inside the object.
}
\usage{
## S4 method for class 'Vegsoup*'
Species(obj)
}

\arguments{
  \item{obj}{
    A \code{Vegsoup*} object.
  }
}

\value{
A \code{"data.frame"} of species occurences and layer replications.
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{Vegsoup}}
}

\keyword{ methods }
