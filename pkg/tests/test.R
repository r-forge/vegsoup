suppressPackageStartupMessages(library(Vegsoup))

setwd("/Users/roli/Documents/vegsoup/pkg/tests")
library(vegsoup)

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
	"Eleocharis palustris", "Caliergonella cuspidata", "Rumex acetosa",
	"Trifolium pratense", "Anthoxanthum odoratum", "Plantago lanceolata",
	"Aira praecox", "Hypochaeris radicata", "Potentilla palustris",
	"Vicia latifolia", "Salix repens", "Empetrum nigrum")

taxonomy <- data.frame(abbr = unique(species$abbr), taxon)
taxonomy <- QueryTaxonomy(x = species, y = taxonomy)

x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")

sites <- SitesWide2SitesLong(x)

#	recode species abundances, assume extend Braun-Blanquet scale
species$cov <- as.character(cut(as.numeric(species$cov),
	0:9, c("r", "+", "1", "2m", "2a", "2b", "3", "4", "5")))

qry <- Vegsoup(x = species, y = sites, z = taxonomy)
dta <- VegsoupData(qry)

std <- Stride(dta, stride = 6, verbose = TRUE)

prt <- VegsoupDataPartition(dta, k = 3, verbose = TRUE, decostand.method = "wisconsin")

VegsoupDataPartition(dta, k = 1, verbose = TRUE, decostand.method = "wisconsin")