\encoding{UTF-8}

\name{SpeciesList}
\docType{methods}

\alias{SpeciesList}
\alias{SpeciesList-methods}
\alias{SpeciesList,Vegsoup-method}

\alias{relevee}
\alias{relevee-methods}
\alias{relevee,Vegsoup-method}

\title{ Return Species List for an \code{Vegsoup*} object }

\description{
Simpliy return a list of taxon names and a corresponding unique key \code{"abbr"} or a listing for a particular plot. 
}

\usage{
\S4method{SpeciesList}{Vegsoup}(obj, layered)

\S4method{relevee}{Vegsoup}(obj, plot, format = FALSE)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ layered }{
    Return species list with layers, defaults to \code{FALSE}. See
    \sQuote{Details}.
  }  
  \item{ format }{
    logical. Format output.
  }
  \item{ plot }{
    Index or character giving a rowname (\code{rownames(obj)}), defaults to \code{1}, the first plot
    in the data set.
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
  The SpeciesList method will not print anything to console.
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

# plot listing
relevee(x, 1)

relevee(x, "bs01") # equal to the above

relevee(x, 1, format = TRUE)
}
\keyword{methods}

