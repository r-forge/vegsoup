\name{abbreviations}
\docType{methods}
\alias{abbr.layer}
\alias{abbr.layer-methods}
\alias{abbr}
\alias{abbr-methods}
\alias{abbr<-}
\alias{abbr<--methods}
\alias{abbr<-,Vegsoup,character-method}

\title{ Get Abbreviations used to Encode Species and Layer Replicates }
\description{
  Get the abbreviations (\emph{abbr}) used in species matrix and corresponding
  taxonomy data frame.
}
\usage{
abbr(obj, ...)

abbr(obj) <- value

abbr.layer(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a \code{Vegsoup*} object.
}
  \item{\dots}{
not used
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

