\name{QueryTaxonomy}
\alias{QueryTaxonomy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare species abbreviations to taxonomic reference list
}
\description{
Given an \code{R} object resulting from \code{SpeciesWide2SpeciesLong} or a csv-file valid for input for \code{SpeciesWide2SpeciesLong}, query the used abbreviation (column \code{abbr}) against a taxonomic reference list (column \code{abbr}).  
}
\usage{
QueryTaxonomy(x, y, file.x, file.y, csv2 = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{a data frame having species abundances in long format. See details}
\item{x}{a data frame giving a taxonomic reference list. See details.}
\item{file.x}{ path to a csv-file stored on disk having species abundances in long format. See details}
\item{file.y}{ path to a csv-file giving a taxonomic reference list. See details}
\item{csv2}{ use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}}
}
\details{
All unique abbreviations found in \code{x} respectively \code{file.x} are matched against \code{y} respectively \code{file.y}. All possible combinations are supported. If \code{verbose = TRUE} the function prints all non-matching strings on the screen.
}
\value{
\code{QueryTaxonomy} returns a two-column \code{data.frame}; the first columns has taxon abbreviations ('abbr'), the second the corresponding scientific names as supplied by the taxonomic reference list.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoup-class},
\link{SpeciesWide2SpeciesLong},
\link{SitesWide2SitesLong}.
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

species <- SpeciesWide2SpeciesLong(x)

#	create taxonomy reference list
#	these are the scientific names corresponding to the abbreviations in \code{dune}

taxon <- c("Bellis perennis", "Leontodon autumnalis", "Poa pratensis",
	"Trifolium repens", "Achillea millefolium", "Poa trivialis",
	"Elymus repens", "Lolium perenne", "Alopecuros geniculatuis",
	"Bormus hordeaceus", "Juncus bufonius", "Ranunculus flammula",
	"Cheopodium album", "Sagina procumbens", "Agrostis stolonifera",
	"Brachytethium rutabulum", "Cirsium arvense", "Juncus articulatus",
	"Eleocharis püalustris", "Caliergonella cuspidata", "Rumex acetosa",
	"Trifolium pratense", "Anthoxanthum odoratum", "Plantago lanceolata",
	"Aira pyramidalis", "Hypochaeris radicata", "Potentilla palustris",
	"Vicia latifolia", "Salix repens", "Empetrum nigrum")

taxonomy <- data.frame(abbr = unique(species$abbr), taxon)

taxonomy <- QueryTaxonomy(x = species, y = taxonomy)

#	leave one out will rise an error!
taxonomy  <- QueryTaxonomy(x = species, y = taxonomy[- 2])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{import}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
