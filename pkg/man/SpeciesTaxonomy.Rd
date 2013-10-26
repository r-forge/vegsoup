\name{taxonomy}
\docType{methods}

\alias{SpeciesTaxonomy}

\title{ Compare Species Abbreviations to Taxonomic Reference List }

\description{
Function \code{SpeciesTaxonomy} accepts local \code{R} objects (arguments \code{x} respectively \code{file.x}) or csv-files (\code{y} respectively \code{file.y}). All meaningfull combinations are supported and each argument is coerced to it's corresponding class. Argument \code{x} is initilaized as class \code{"Species"} and \code{y} as class \code{"Taxonomy"}, including basic data integrity checking for the respective class. The function further checks if the abbreviation in \code{x} (class \code{"Species"} with column \code{'abbr'}) matches those in \code{y} (class \code{"Taxonomy"}, itself with column \code{'abbr'}). Any mismatches are printed on screen, and possibly no object is returned if serious deviations where identified.
}

\usage{
  SpeciesTaxonomy(x, y, file.x, file.y, sep = ";", dec = ",", pmatch = FALSE, skip = TRUE, verbose = FALSE, ...)
}

\arguments{
  \item{ x }{
    A \code{"data.frame"} or \code{"matrix"} conforming to the specifications
    of class \code{\linkS4class{Species}} or an object of that class.
  }
  \item{ y }{
    A \code{"data.frame"} or \code{"matrix"} conforming to the specifications
    of class \code{\linkS4class{Taxonomy}} or an object of that class.
  }
  \item{ file.x }{
    Path to a csv-file having species abundances in long format as defined for
    \code{\linkS4class{Species}}.
  }
  \item{ file.y }{
    Path to a csv-file giving a taxonomic reference list as defined for
    \code{\linkS4class{Taxonomy}}.
  }
  \item{ sep, dec }{
    Seperators and decimals in input file, see \code{\link{read.csv}}. Defaults to the behavior
	of \code{read.csv2}!
  }
  \item{ pmatch }{
    use \code{pmatch} to match abbreviation in \code{x} respectively
    \code{file.x} against the reference. See \sQuote{Details}.
  }
  \item{ skip }{
    Retain only mandatory columns, defaults to \code{TRUE}.
  }
  \item{ verbose }{
    Prints Diagnostic messages.
  }
  \item{ \dots }{
    Not used.
  }
}

\details{
  Of course the column \code{'abbr'} in the taxonomic reference has to be
  unique. Not only because it is used as rownames, but also as to reference a
  scientific name to only one abbreviation.

  Classes \code{\linkS4class{Species}} and \code{\linkS4class{Taxonomy}}
  allow more columns than the default and mandatory ones. If argument
  \code{skip = TRUE} all non nessecary columns will be skipped and the
  minimal set of columns is retained. Set \code{skip} to \code{TRUE} if you
  wish to keep the information of additional columns.

  If an abbreviation used in the object representing species observations
  (class \code{"Species"}) is not found in the reference (class
  \code{"Taxonomy"}), and vice-versa, the function stops and a
  \code{"data.frame"} including all ambiguities is returned. If argument
  \code{pmatch = TRUE} partial matches are treated as correct
  (see \link{pmatch}). Proceed with fingers crossed! Never the less,
  if you are happy with what \code{pmatch} found and you want to use these
  matches to overriede your raw data, you might think about saving the object
  returned from \code{SpeciesTaxonomy(...)} to disk and run the function again
  on these modified objects. If there are any matching errors or ambiguities
  it is highly advisable, however, to review the data and apply corrections
  manually to the raw data where necessary.
}
\value{
  An object of class \code{\linkS4class{SpeciesTaxonomy}} with slots of class
  \code{\linkS4class{Species}} and \code{\linkS4class{Taxonomy}}.
}
\note{
  Integrity of the taxonomic reference list is crucial to build an object of
  class \code{"SpeciesTaxonomy"} and function \code{Vegsoup} also relies on
  it.  If it's necessary to replace several abbreviations it is handy to save
  \code{write.table(taxonomy(obj), "foo.txt", quote = FALSE, row.names =
  FALSE)} to create an object suitable for automatic replacement.
}

\author{ Roland Kaiser }

\seealso{
\code{\linkS4class{Species}},
\code{\link{stack.species}},
\code{\linkS4class{Taxonomy}}
}

\examples{
library(vegsoup)
library(vegan)
data(dune)
data(dune.env)

# create species data
# there are two moss species in the dune data set
# see vector of taxon names below
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groome plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

spc <- stack.species(x)

# create taxonomy reference list from scratch
# these are the scientific names corresponding to the abbreviations in dune
# as I understand it

taxon <- c("Bellis perennis", "Leontodon autumnalis", "Poa pratensis",
	"Trifolium repens", "Achillea millefolium", "Poa trivialis",
	"Elymus repens", "Lolium perenne", "Alopecuros geniculatuis",
	"Bormus hordeaceus", "Juncus bufonius", "Ranunculus flammula",
	"Cheopodium album", "Sagina procumbens", "Agrostis stolonifera",
	"Brachytethium rutabulum", "Cirsium arvense", "Juncus articulatus",
	"Eleocharis palustris", "Caliergonella cuspidata", "Rumex acetosa",
	"Trifolium pratense", "Anthoxanthum odoratum", "Plantago lanceolata",
	"Aira praecox", "Hypochaeris radicata", "Potentilla palustris",
	"Vicia latifolia", "Salix repens", "Empetrum nigrum")

txa <- data.frame(abbr = unique(slot(spc, "data")$abbr), taxon)

txa <- new("Taxonomy", data = txa)

spc.txa <- SpeciesTaxonomy(x = spc, y = txa)

}

\keyword{import}