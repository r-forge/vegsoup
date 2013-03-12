\name{abbreviations}
\docType{methods}
\alias{abbr.layer}
\alias{abbr.layer-methods}
\alias{abbr}
\alias{abbr-methods}
\alias{abbr<-}
\alias{abbr<--methods}

\title{ Get Abbreviations used to Encode Species and Layer Replicates }
\description{
  Get the abbreviations (\emph{abbr}) used in species matrix and corresponding
  taxonomy data frame.
}
\usage{
## S4 method for signature 'Vegsoup*'
abbr(obj)
## S4 replacement method for signature 'Vegsoup*'
abbr(obj) <- value
## S4 method for signature 'Vegsoup*'
abbr.layer(obj)
}

\arguments{
  \item{ obj }{
    a \code{Vegsoup*} object.
  }
  \item{ value }{
    Character. Usefull to change abbreviations.a \code{Vegsoup*} object.
  }
}
\details{
  The design of package vegsoup with regard to layer replication is to encode
  species and layer in a collpased string that is then used as column names in
  a species matrix. This so avoids the creation of multi dimensional arrays.
}
\value{
returns a vector of mode \code{"character"}.
}
\author{
Roland Kaiser
}
\examples{
library(vegsoup)

dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
abbr(dta)
}
\seealso{
\code{\link{split.abbr}}
\code{\link{Layers}}
\code{\link{as.array}}
}
\keyword{ method }

