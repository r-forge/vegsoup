setClass("SpeciesTaxonomy",
	representation(
		species = "Species",
		taxonomy = "Taxonomy"
	)
)

setMethod("species",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) species(slot(obj, "species")) # ? slot(obj, "species")
)
setMethod("taxonomy",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) taxonomy(slot(obj, "taxonomy")) # ? slot(obj, "species")
)
setGeneric("species<-",
	function (obj, value, ...)
		standardGeneric("species<-")
)
setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "Species"),
	function (obj, value) {
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
		species = value,
		taxonomy = taxonomy(taxonomy(obj)[sel, ]))
	}
)
setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "data.frame"),
	function (obj, value) {
		value <- species(value)
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
		species = value,
		taxonomy = taxonomy(taxonomy(obj)[sel, ]))
	}
)
setGeneric("taxonomy<-",
	function (obj, value, ...)
		standardGeneric("taxonomy<-")
)
setReplaceMethod("taxonomy",
	signature(obj = "SpeciesTaxonomy", value = "Taxonomy"),
	function (obj, value) {
		x <- value$abbr
		y <- species(obj)$abbr
		sel <- unlist(sapply(x, function (x) which(x == y)))
		new("SpeciesTaxonomy",	
		species = species(species(obj)[sel, ]),
		taxonomy = value)
	}
)
setReplaceMethod("taxonomy",
	signature(obj = "SpeciesTaxonomy", value = "data.frame"),
	function (obj, value) {
		value <- taxonomy(value)
		x <- value$abbr
		y <- species(obj)$abbr
		sel <- unlist(sapply(x, function (x) which(x == y)))
		new("SpeciesTaxonomy",
		species = species(species(obj)[sel, ]),
		taxonomy = value)
	}
)
".rbind.SpeciesTaxonomy" <- function (..., deparse.level = 1) {
	allargs <- list(...)
	#allargs <- list(obj1, obj2)
	x <- do.call("rbind", sapply(lapply(allargs, species), species))
	z <- do.call("rbind", sapply(lapply(allargs, taxonomy), taxonomy))
	return(SpeciesTaxonomy(x, z))
}
#	Sites, Taxonomy Vegsoup have also rbind method
if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
}
setMethod("rbind",
    signature(... = "SpeciesTaxonomy"),
	.rbind.SpeciesTaxonomy
)	 	  
#setMethod("[",
#    signature(x = "SpeciesTaxonomy",
#    i = "ANY", j = "ANY", drop = "missing"),
#    function (x, i, j, ..., drop = FALSE) {
#    	species(x@data[i, j, ...])
#    }
#)    	

#	initialize method is wrapped in function SpeciesTaxonomy

#setClass("SpeciesTaxonomyVirtual",
#	representation = representation("VIRTUAL")	
#)
