\encoding{UTF-8}

\name{abbreviations}
\docType{methods}

\alias{abbr}
\alias{abbr,Vegsoup-method}
\alias{abbr,Species-method}
\alias{abbr,Taxonomy-method}

\alias{abbr.layer}
\alias{abbr.layer,Vegsoup-method}

\alias{abbr<-}
\alias{abbr<-,Vegsoup-method}
\alias{abbr<-,Vegsoup,Character-method}

\title{ Get Abbreviations used to Encode Species and Layer Replicates }

\description{
  Get the abbreviations (\emph{abbr}) used to encode scientific species names. There are methods
  for \code{"Species"} and \code{"Taxonomy"} classes, as well as for \code{Vegsoup} objects.
}

\usage{
\S4method{abbr}{Vegsoup}(obj)

\S4method{abbr}{Vegsoup}(obj) <- value

\S4method{abbr.layer}{Vegsoup}(obj)

\S4method{abbr}{Species}(obj)

\S4method{abbr}{Taxonomy}(obj)
}

\arguments{
  \item{ obj }{
	a \code{Vegsoup*}, \code{Species} or \code{Taxonomy} object.
  }
  \item{ value }{
	character. Useful to change abbreviations.
  }
}
\details{
  For \code{'vegsoup'} objects \code{abbr} returns \code{sort(unique(species(obj)$abbr))} and
  for \code{Species} or \code{Taxonomy} objects the method returns column abbr from \emph{slot 'data'}.
}
\value{
\code{character}.
}
\author{
Roland Kaiser
}
\examples{
require(vegsoup)
data(barmstein)

abbr(barmstein)
}
\seealso{
\code{\link{splitAbbr}}
\code{\link{layers}}
\code{\link{as.array}}
}
\keyword{ method }

