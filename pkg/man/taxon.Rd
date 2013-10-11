\name{taxon}
\alias{taxon}
\alias{taxon-method}
\alias{subset}
\alias{subset-methods}

\title{ Get or Subset Object by Scientific Taxon Name }
\description{
  Retrieve a \code{character} vector of scientific taxon names as defined by
  \code{Taxonomy(x)} or subset an object based on these names such that
  only a subset of species remain.
}
\usage{
\S4method{taxon}{Vegsoup}(x)
\S4method{subset}{Vegsoup}(x, subset, ...)
}

\arguments{
  \item{ x }{
    A \code{Vegsoup*} object.
  }
  \item{ subset }{
    \code{numeric} index to \code{Taxonomy(x)}, vector of mode \code{character}, or a logical
    expression.
  }
  \item{ \dots }{
    Arguments passed to \code{\link{grep}}.
  }
}
\details{
  Function \code{taxon} retrieves scientific species names from an object.

  Extending the functionality of the \code{\link[vegsoup:extract]{"["}} extract methods for
  vegsoup objects, that allow for a definition of subsets based on abbreviations as
  returned by \code{\link[vegsoup:colnames]{colnames}}, this set of methods provides other
  means of selecting species.

  By using the the \code{subset}-method it becomes possible to select species based on their
  taxonomic classification or other traits that go along with species names.

  If subset is of mode \code{character} the string is passed to function \code{grep} and seeked
  in column \code{'taxon'} of \code{Taxonomy(x)}. Argument \code{"subset"} can be a vector of
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
\code{\link[vegsoup:extract]{"["}}
}
\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")

head(taxon(dta)) # short-hand for head(Taxonomy(dta)$taxon)

#  subset object using a species scientific name
#  and use function taxon() for display
taxon(subset(dta, "Carex"))
#  pass argument to grep()
#  note, if invert = TRUE the selected subset is inverted
#  and all species not matching 'Carex' are retained!
taxon(subset(dta, "carex", invert = TRUE, ignore.case = TRUE))

#  alternative if Taxonomy(dta)$abbr has meaningful abbreviations
taxon(dta[, grep("care" , colnames(dta))])

#  numeric index to define subset
x <- sample(1:length(taxon(dta)), 10)
colnames(subset(dta, x)) # colnames() displays column names of as.matrix(dta)

## some more usage examples
#  subset object such that only the set of species of the most species rich sample remain
res <- subset(dta, taxon(dta[which.max(richness(dta, "sample")), ]))
dim(res)

#  set of species that occur in all relevees (plots)
res <- subset(dta, taxon(dta[, colSums(dta) == nrow(dta)]))
dim(res)

#  logical index to define subset
#  get family retrieved from the plant list
library(Taxonstand)

tmp <- TPL(taxon(dta))
#  Carex is a genus of Cyperaceae, let's see if there are more mebers of this family
taxon(subset(dta, tmp$Family == "Cyperaceae")) # no, there are not
}
\keyword{ methods }

