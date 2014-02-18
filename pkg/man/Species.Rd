\name{Species}
\docType{methods} 

\alias{Species}
\alias{Species,Vegsoup-method}

\alias{Species<-}
\alias{Species<-,Vegsoup-method}
\alias{Species<-,Vegsoup,Species-method}
\alias{Species<-,Vegsoup,SpeciesTaxonomy-method}
\alias{Species<-,Vegsoup,data.frame-method}

\title{ Methods to Retrieve or Manipulate the Species Slot of \code{Vegsoup*} Objects }
\description{
Accessor method to retrieve the species data in \emph{long format} as stored inside the object.
}
\usage{
\S4method{Species}{Vegsoup}(obj)
\S4method{Species}{Vegsoup,Species}(obj) <- value
\S4method{Species}{Vegsoup,SpeciesTaxonomy}(obj) <- value
\S4method{Species}{Vegsoup,data.frame}(obj) <- value
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ value }{
    Not yet implemented.
  }  
}

\value{
A \code{"data.frame"} of species occurences and layer replications.
}

\author{ Roland Kaiser }

\seealso{
\code{\linkS4class{Vegsoup}}
}

\examples{
require(vegsoup)

data(barmstein)

# retrieve data.frame from an Vegsoup object
spc <- Species(barmstein)
class(spc)
}
\keyword{ methods }

