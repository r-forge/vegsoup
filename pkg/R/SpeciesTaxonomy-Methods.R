#	replace taxonomy including species abbreviations

#setGeneric("SpeciesTaxonomy",
#	function (x, y, file.x, file.y, sep = sep, dec = dec, pmatch = FALSE, skip = TRUE, verbose = FALSE, ...)
#		standardGeneric("SpeciesTaxonomy")
#)

setMethod("taxonomy",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) taxonomy(slot(obj, "taxonomy"))
)

setReplaceMethod("taxonomy",
	signature(obj = "SpeciesTaxonomy", value = "Taxonomy"),
	function (obj, value) {
		x <- value$abbr # taxonomy
		y <- species(obj)$abbr
		#	keep order!
		sel <- logical(length(y))		
		sel[unlist(sapply(x, function (x) which(x == y)))] <- TRUE
		new("SpeciesTaxonomy",	
			species = species(species(obj)[sel, ]),
			taxonomy = value)
	}
)

setReplaceMethod("taxonomy",
	signature(obj = "SpeciesTaxonomy", value = "data.frame"),
	function (obj, value) {
	x <- value$abbr # taxonomy
		y <- species(obj)$abbr
		#	keep order!
		sel <- logical(length(y))		
		sel[unlist(sapply(x, function (x) which(x == y)))] <- TRUE
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
