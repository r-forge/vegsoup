\name{split.abbr}
\docType{methods}
\alias{split.abbr}
\alias{split.abbr-methods}

\title{
Deparse Species Names of Objects of Class Vegsoup*
}
\description{
Deparse the layer replications (pseudo-species) used as \code{colnames} of a species matrix returned from \code{Vegsoup*} objects into components.
}
\usage{
## S4 getter method for 'Vegsoup*' objects
split.abbr(obj, verbose = FALSE)
}
\arguments{
  \item{ obj }{
    An \code{Vegsoup*} object.
  }
  \item{ verbose }{
    Additional output.
  }
}
\details{
  The method decomposes the \code{colnames} of the species matrix into three
  components. The first giving the taxon abbreviation only (\code{'abbr'}),
  the second the layer code (\code{'layer'}) and the last component
  corresponds to the full taxon name (\code{'taxon'}) as queryied from
  \code{Taxonomy(obj)}.

  The compound taxon key (\code{'abbr.layer'}) can be accessed from the
  \code{rownames} of the resulting object. This is advantegous as it provides
   an easy means of subsetting with \code{colnames(obj)}.

  \code{\link{Vegsoup}} and \code{\linkS4class{Species}} construct these
  \emph{compound taxon abbreviations} by pasting \code{make.names(x)} and
  \code{layer} seperated with \code{"@"}. To deparse the abbreviation and
  layer string \code{split.abbr} should be used.
}
\value{
  A \code{"data.frame"} with as many rows and in the same order as there are
  columns in the species matrix. Note, the method uses \code{invisible} when
  returning it's results.
}
\author{ Roland Kaiser }
\seealso{
\code{\link{Vegsoup}}
\code{\link{Taxonomy}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
x <- split.abbr(dta)

head(txa)
all.equal(rownames(x), colnames(dta))

#   get scientific taxa names
m <- as.matrix(dta, typeof = "character", mode = "R")
x[rownames(m), ]

cbind(x[rownames(m), ], m)
}
\keyword{ methods }
