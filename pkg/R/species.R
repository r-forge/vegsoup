#	rename
#	class Species defines setGeneric("species")
#	get species in long format
setGeneric("Species",
	function (obj)
		standardGeneric("Species")
)
setMethod("Species",
    signature(obj = "Vegsoup"),
    function (obj) obj@species
)
setGeneric("Species<-",
	function (obj, value)
		standardGeneric("Species<-")
)
setReplaceMethod("Species",
	signature(obj = "Vegsoup", value = "SpeciesTaxonomy"),
	function (obj, value) {
		warning("not implemented yet")
		return(obj)		
	}
)

#if (!isGeneric("SpeciesList")) {
setGeneric("SpeciesList",
	function (obj, layered)
		standardGeneric("SpeciesList")
)
#}
setMethod("SpeciesList",
    signature(obj = "Vegsoup"),
    function (obj, layered = FALSE) {
    	if (missing(layered)) {
    		layered <- FALSE
    	}
    	if (layered) {
	    	res <- Species(obj)
    		res <- unique(res[c("abbr", "layer")])
    		res$taxon <- Taxonomy(obj)[res$abbr, ]$taxon
	    	res <- res[, c("abbr", "taxon", "layer")]
	    	res <- res[order(res$layer, res$taxon),]	    				
    	} else {
    		res <- Taxonomy(obj)[]	
    	}
    	rownames(res) <- seq_len(nrow(res))
    	return(invisible(res))	
	}
)
