\name{Species}
\docType{methods} 
\alias{Species}
\alias{Species-methods}
\alias{Species,Vegsoup-method}
\alias{Species<-}
\alias{Species<--methods}
\alias{Species<-,Vegsoup,data.frame-method}

\title{
Methods to Retrieve or Manipulate the Species Slot Vegsoup* Objects
}
\description{
Accessor method to retrieve the species data in \emph{long format} as stored inside the object.
}
\usage{
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
