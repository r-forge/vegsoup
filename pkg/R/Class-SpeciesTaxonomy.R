setClass("SpeciesTaxonomy",
	representation(
		species = "Species",
		taxonomy = "Taxonomy"
	)
)

#	accessor method
setMethod("species",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) species(slot(obj, "species"))
)

setMethod("taxonomy",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) taxonomy(slot(obj, "taxonomy"))
)
#A virtual class is a class for which it is not possible to create objects!
# but methods can be defined

#setClass("SpeciesTaxonomyVirtual",
#	representation = representation("VIRTUAL")	
#)
