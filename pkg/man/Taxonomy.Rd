\name{taxonomy}
\docType{methods} 
\alias{SpeciesTaxonomy}
\alias{Taxonomy}
\alias{Taxonomy-methods}
\alias{Taxonomy,Vegsoup-method}
\alias{Taxonomy<-}
\alias{Taxonomy<--methods}
\alias{Taxonomy<-,Vegsoup,data.frame-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare Species Abbreviations to Taxonomic Reference List}
\description{
Given an \code{R} object or a csv-file valid for input for \code{stack.species}, query the used abbreviation (column \code{'abbr'}) against a taxonomic reference list (column \code{'abbr'}).  
}
\usage{
SpeciesTaxonomy(x, y, file.x, file.y, csv2 = TRUE, pmatch = FALSE, verbose = FALSE)

Taxonomy(obj)

Taxonomy(obj) <- value 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ x }{
A data frame conforming to the specifications of class \code{\linkS4class{Species}} or an object of that class. 
}
  \item{ y }{
A data frame conforming to the specifications of class \code{\linkS4class{Taxonomy}} or an object of that class.
}
  \item{ file.x }{
Path to a csv-file having species abundances in long format as defined for \code{\linkS4class{Species}}.
}
  \item{ file.y }{
Path to a csv-file giving a taxonomic reference list as defined for \code{\linkS4class{Taxonomy}}.
}
  \item{ csv2 }{
  use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}.
}
  \item{ pmatch }{
use \code{pmatch} to match abbreviation in \code{x} respectively \code{file.x} against the reference, See details.
}
  \item{ verbose }{
Prints Diagnostic messages.
}
}
\details{
All unique abbreviations found in \code{x} respectively \code{file.x} are matched against \code{y} respectively \code{file.y}. All possible combinations are supported. If \code{verbose = TRUE} the function prints all non-matching strings on the screen. If an abbreviation in the species data frame is not found in the reference list the function stops. If argument \code{pmatch = TRUE} partial matches are treated as correct (see \link{pmatch}). Proceed with finger crossed! It is advisable, however, to review the data and make corrections in the raw data where necessary. If you are happy with what patch found and want this to override your raw data, you need to save \code{QueryTaxonomy(...)\$species to disk.}. If it's nessacary to replace several abbreviations use \code{write.table(taxonomy, "foo.txt", quote = FALSE, row.names = FALSE)} to create an object suitable for replacement.
}
\value{
An object of class \code{"SpeciesTaxonomy"} with slots \code{"species"} and \code{"taxonomy"}.

\item{\code{ slot(taxonomy, "data") }}{a \code{data.frame} with at least two columns; the first column, aswell as the rownames, contain taxon abbreviations \code{'abbr'}, the second column has the corresponding scientific names as supplied by the taxonomic reference list.}
\item{\code{ slot(species, "data") }}{ a data frame with at least four columns and valid taxon abbreviations (\code{'abbr'}) compared to slot(taxonomy, "data"). See \link{Vegsoup-class} and \link{stack.species} for details.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Species-class},
\link{Taxonomy-class},
\link{stack.species},
\link{stack.sites}
}
\examples{
library(vegsoup)
library(vegan)
data(dune)
data(dune.env)

#	create species data
#	there are two moss species in the dune data set
#	see vector of taxon names below ()
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groome plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

spc <- stack.species(x)

#	create taxonomy reference list
#	these are the scientific names corresponding to the abbreviations in dune

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

spc.tax <- SpeciesTaxonomy(x = spc, y = txa)

}

\keyword{import}

