\name{SpeciesList}
\alias{SpeciesList}
\alias{SpeciesList-method}
\alias{taxon}
\alias{taxon-method}

\title{ Return Species List for an \code{Vegsoup*} object }
\description{
Simpliy return a list of taxon names and a corresponding unique key \code{"abbr"}. 
}
\usage{
  SpeciesList(obj, layered)
  taxon(obj)
}
\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ layered }{
    Return species list with layers, defaults to \code{FALSE}. See
    \sQuote{Details}.
}
}
\details{
  If \code{layered = TRUE} also return all layers where a species occurs.
}
\value{
  A \code{"data.frame"} with two columns if \code{layered = FALSE}, or three
  columns if \code{layered = FALSE}. The first giving the unique taxon key
  (\code{'abbr'}), the second the full taxon name \code{'taxon'}, and the last
  the layer (\code{'layer'}) where the taxon occurs in.
}
\note{
  The method will not print anything to console.
}
\author{ Roland Kaiser }
\examples{
library(vegsoup)
data(testdata)

dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")

#	including abbreviations
sl <- SpeciesList(dta)
head(sl)

sl <- SpeciesList(dta, layered = TRUE)
head(sl)

#	scientific taxon names only
taxon(dta)
}
\keyword{methods}