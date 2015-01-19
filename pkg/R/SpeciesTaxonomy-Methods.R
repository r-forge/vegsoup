setMethod("taxonomy",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) slot(obj, "taxonomy")
)

setReplaceMethod("taxonomy",
	signature(obj = "SpeciesTaxonomy", value = "Taxonomy"),
	function (obj, value) {
		x <- value$abbr # taxonomy
		y <- species(obj)$abbr
		#	keep order!
		i <- logical(length(y))	
		i[unlist(sapply(x, function (x) which(x == y)))] <- TRUE
		new("SpeciesTaxonomy",	
			species = species(obj)[i, ],
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
		x <- do.call("rbind", lapply(allargs, species))
		z <- do.call("rbind", lapply(allargs, taxonomy))
		return(SpeciesTaxonomy(x, z))
	}
)	 	  

setMethod("$",
    signature(x = "SpeciesTaxonomy"),
	function(x, name) {
		if (!("species" %in% slotNames(x))) {
			stop("no $ method for object without slot species")
		}
		return(species(x@species)[[name]])
	}
)

setMethod("[",
    signature(x = "SpeciesTaxonomy",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
    	if (!missing(j)) message("ignore argument j")
    	j <- rep(TRUE, ncol(species(x)))
    	return(SpeciesTaxonomy(x@species[i, j], x@taxonomy))		    	
    }
)

#setMethod("[",
#    signature(x = "SpeciesTaxonomy",
#    i = "ANY", j = "ANY", drop = "missing"),
#    function (x, i, j, ..., drop = FALSE) {
#    	species(x@data[i, j, ...])
#    }
#)
