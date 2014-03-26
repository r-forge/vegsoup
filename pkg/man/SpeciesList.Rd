\encoding{UTF-8}

\name{SpeciesList}
\docType{methods}

\alias{SpeciesList}
\alias{SpeciesList-methods}
\alias{SpeciesList,Vegsoup-method}

\title{ Return Species List for an \code{Vegsoup*} object }
\description{
Simpliy return a list of taxon names and a corresponding unique key \code{"abbr"}. 
}
\usage{
\S4method{SpeciesList}{Vegsoup}(obj, layered)
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
require(vegsoup)

data(barmstein)
x <- barmstein

# including abbreviations
l <- SpeciesList(x)
head(l)

l <- SpeciesList(x, layered = TRUE)
head(l)

# scientific taxon names only
taxon(x)
}
\keyword{methods}

