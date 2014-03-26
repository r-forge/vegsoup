\encoding{UTF-8}

\name{ collapse abbreviations }
\docType{methods}

\alias{splitAbbr}
\alias{splitAbbr,Vegsoup-method}

\title{ Deparse Species Layer Replication of \code{Vegsoup*} Objects }

\description{
  Deparse the layer replications (pseudo-species) used as \code{colnames} in the
  species matrix of \code{Vegsoup*} objects into it's components.
}

\usage{
\S4method{splitAbbr}{Vegsoup}(obj)
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
  corresponds to the full taxon name (\code{'taxon'}) as queried from
  \code{Taxonomy(obj)}.

  The compound taxon key (\code{'abbr.layer'}) can be accessed from the
  \code{rownames} of the resulting object. This is advantageous as it provides 
  an easy means of subsetting with \code{colnames(obj)}.

  \code{\link{Vegsoup}} and \code{\linkS4class{Species}} construct these
  \emph{compound taxon abbreviations} by pasting \code{make.names(x)} and
  \code{layer} seperated with \code{"@"}. To deparse the abbreviation and
  layer string \code{splitAbbr} should be used.
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
require(vegsoup)

data(barmstein)
x <- barmstein

# deparse compund taxon and layer replicates
a <- splitAbbr(x)
head(a)

all.equal(rownames(a), colnames(x))

# get scientific taxa names for the columns of the species matrix
# as.matrix returns a species matrix from an 'Vegsoup' object
# we use mode = "R" for instantaneous matrix transposition 
m <- as.matrix(x, typeof = "character", mode = "R")
head(a[rownames(m), ])

# for a unique list of taxa
taxon(x)

# similar result but a matrix
head(SpeciesList(x, layered = TRUE))

# species matrix including taxon names and layers
m <- cbind(a[rownames(m), ], m)

# and this can be converted bach to class 'Species'
( stackSpecies(m) )

# indexing of 'Vegsoup' object by suppling indices of the the species matrix
# elements according to 'as.matrix' method
x[, rownames(a)[sample(1:ncol(x), 10)]]
}

\keyword{ methods }

