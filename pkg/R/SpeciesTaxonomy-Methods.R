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

setMethod("rbind",
    signature(... = "SpeciesTaxonomy"),
	function (..., deparse.level = 1) {
		allargs <- list(...)
		x <- do.call("rbind", sapply(lapply(allargs, species), species))
		z <- do.call("rbind", sapply(lapply(allargs, taxonomy), taxonomy))
		return(SpeciesTaxonomy(x, z))
	}
)	 	  

#setMethod("[",
#    signature(x = "SpeciesTaxonomy",
#    i = "ANY", j = "ANY", drop = "missing"),
#    function (x, i, j, ..., drop = FALSE) {
#    	species(x@data[i, j, ...])
#    }
#)
