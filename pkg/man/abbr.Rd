\name{abbreviations}
\docType{methods}

\alias{abbr.layer}
\alias{abbr.layer,Vegsoup-method}

\alias{abbr}
\alias{abbr,Vegsoup-method}

\alias{abbr<-}
\alias{abbr<-,Vegsoup-method}
\alias{abbr<-,Vegsoup,Character-method}

\title{ Get Abbreviations used to Encode Species and Layer Replicates }

\description{
  Get the abbreviations (\emph{abbr}) used in species matrix and corresponding
  taxonomy data frame.
}

\usage{
\S4method{abbr}{Vegsoup}(obj)

\S4method{abbr}{Vegsoup}(obj)<-value

\S4method{abbr.layer}{Vegsoup}(obj)
}

\arguments{
  \item{ obj }{
    a \code{Vegsoup*} object.
  }
  \item{ value }{
    character. Usefull to change abbreviations.
  }
}
\details{
  The design of package vegsoup with regard to layer replication is to encode
  species and layer in a collpased string that is then used as column names in
  the species matrix. This so avoids the creation of multi dimensional arrays.
}
\value{
\code{character}. A vector of column names.
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
\code{\link{Layers}}
\code{\link{as.array}}
}
\keyword{ method }

