setClass("SpeciesTaxonomy",
	representation(
		species = "Species",
		taxonomy = "Taxonomy"
	)
)

#setClassUnion("SpeciesTaxonomy",
#	c("Species", "Taxonomy")
#)