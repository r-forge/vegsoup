library(vegsoup)
library(vegan)
data(dune)
data(dune.env)
#	there are two moss species in the dune data set
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groome plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

species <- SpeciesWide2SpeciesLong(x)

x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")

sites <- SitesWide2SitesLong(x)

taxon <- c("Bellis perennis", "Leontodon autumnalis", "Poa pratensis",
	"Trifolium repens", "Achillea millefolium", "Poa trivialis",
	"Elymus repens", "Lolium perenne", "Alopecuros geniculatuis",
	"Bormus hordeaceus", "Juncus bufonius", "Ranunculus flammula", "Cheopodium album",
	"Sagina procumbens", "Agrostis stolonifera", "Brachytethium rutabulum", "Cirsium arvense",
	"Juncus articulatus", "Eleocharis püalustris", "Caliergonella cuspidata",
	"Rumex acetosa", "Trifolium pratense", "Anthoxanthum odoratum",
	"Plantago lanceolata", "Aira pyramidalis", "Hypochaeris radicata",
	"Potentilla palustris", "Vicia latifolia", "Salix repens", "Empetrum nigrum")
taxonomy <- data.frame(abbr = unique(species$abbr), taxon)

taxonomy <- QueryTaxonomy(x = species, y = taxonomy)

res <- Vegsoup(species, sites, taxonomy), scale = "frequency")
#	leave one out will rise an error!
#	taxonomy  <- QueryTaxonomy(x = species, y = taxonomy[- 2])
