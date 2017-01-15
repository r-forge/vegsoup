\encoding{UTF-8}

\name{taxon}
\docType{methods}

\alias{taxon}
\alias{taxon,Vegsoup-method}

\alias{subset}
\alias{subset,Vegsoup-method}

\title{ Get or Subset Object by Scientific Taxon Name }

\description{
  Print or retrieve a character vector of full scientific latin names of taxa
  as defined by \code{taxonomy(x)}. Alternatively, subset an object based on
  taxon names, such that only the selected species, or only those plots where
  selected species occurs in, remain in the object.
}

\usage{
\S4method{taxon}{Vegsoup}(x, taxon = NULL)

\S4method{subset}{Vegsoup}(x, subset, drop = TRUE, ...)
}

\arguments{
  \item{ x }{
    \code{Vegsoup*} object.
  }
  \item{ taxon }{
    \code{character} string of a full scientific latin name (see \code{\link{taxonomy}}).
  }
  \item{ subset }{
    \code{numeric} index to \code{taxonomy(x)}, vector of mode
    \code{character}, or a logical expression.
  }
  \item{ drop }{
    \code{logical}, if \code{TRUE} only the selected species remain.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{grep}}.
  }
}

\details{
  Function \code{taxon} retrieves scientific species names from an object.
  
  If argument taxon is supplied only those taxa returned by a call to \code{grep(taxon, taxon(x))]} are printed.

  Extending the functionality of the \code{\link[vegsoup]{Extract}} methods for
  vegsoup objects, that allow for a definition of subsets based on abbreviations as
  returned by \code{\link[vegsoup]{colnames}}, this set of methods provides other
  means of selecting species.

  By using the the \code{subset}-method it becomes possible to select species based on their
  taxonomic classification or other traits that go along with species names.

  If subset is of mode \code{character} the string is passed to function \code{grep} and seeked
  in column \code{'taxon'} of \code{taxonomy(x)}. Argument \code{"subset"} can be a vector of
  length 1 or more.
  
%%The design of package vegsoup with regard to layer replication is to encode
%%species and layer in a collpased string that is then used as column names in
%%a species matrix. This so avoids the creation of multi dimensional arrays.
}
\value{
A \code{Vegsoup*} object.
}
\author{ Roland Kaiser }
\seealso{
\code{\link[vegsoup]{Extract}}
}
\examples{
data(barmstein)
x <- barmstein

head(taxon(x)) # short-hand for head(taxonomy(x)$taxon)

# display all taxa staring with letter C
taxon(x, "C")
 
# subset object using a species scientific name
# and use function taxon() for display
taxon(subset(x, "Carex")) # drop = TRUE is the default
taxon(subset(x, "Carex", drop = FALSE))

# pass argument to grep()
# note, if invert = TRUE the selected subset is inverted
# and all species not matching 'Carex' are retained!
taxon(subset(x, "carex", invert = TRUE, ignore.case = TRUE))

# alternative if taxonomy(x)$abbr has meaningful abbreviations
taxon(x[ , grep("care", colnames(x)) ])

# numeric index to define subset
i <- sample(1:length(taxon(x)), 10)
colnames(subset(x, i)) # colnames() displays column names of as.matrix(x)

# some more usage examples
# subset object such that only the set of species of the most species rich
# sample remain
res <- subset(x, taxon(x[ which.max(richness(x, "sample")), ]))
dim(res)

# set of species that occur in all relevees (plots)
res <- subset(x, taxon(x[, colSums(x) == nrow(x) ]))
dim(res)

# logical index to define subset
# get family retrieved from the plant list
\dontrun{
library(Taxonstand)
tmp <- TPL(taxon(x))
# Carex is a genus of Cyperaceae, let's see if there are more members of
# this family
taxon(subset(x, tmp$Family == "Cyperaceae")) # no, there are not
}
}
\keyword{ methods }

