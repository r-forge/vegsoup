\name{abbreviations}
\docType{methods}

\alias{split.abbr}
\alias{split.abbr,Vegsoup-method}

\title{ Deparse Species Layer Replication of \code{Vegsoup*} Objects }

\description{
  Deparse the layer replications (pseudo-species) used as \code{colnames} in the
  species matrix of \code{Vegsoup*} objects into it's components.
}

\usage{
\S4method{split.abbr}{Vegsoup}(obj)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
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
\code{\linkS4class{Taxonomy}}
\code{\link{SpeciesList}}
\code{\link{taxon}}
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
head(x[rownames(m), ])

#	similar result but without meaningfull rownames
head(SpeciesList(dta, layered = TRUE))

#	species matrix including taxon anmes and layers
head(cbind(x[rownames(m), ], m))

res <- dta[, rownames(split.abbr(dta))[sample(1:ncol(dta), 10)]]
}

\keyword{ methods }
