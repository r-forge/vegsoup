if (!isGeneric("nrow")) {
setGeneric("nrow", function (x)
	standardGeneric("nrow"))
}

setMethod("nrow",
    signature(x = "Vegsoup"),
    function (x) {
		nrow(Sites(x))
	}
)

setMethod("nrow",
    signature(x = "Species"),
    function (x) {
		nrow(species(x))
	}
)

setMethod("nrow",
    signature(x = "Sites"),
    function (x) {
		nrow(sites(x))
	}
)

setMethod("nrow",
    signature(x = "Taxonomy"),
    function (x) {
		nrow(slot(x, "data"))
	}
)


if (!isGeneric("ncol")) {
setGeneric("ncol", function (x)
	standardGeneric("ncol"))
}

setMethod("ncol",
    signature(x = "Vegsoup"),
    function (x) {
    	if (length(Layers(x)) > 1) {
    		nrow(unique(species(species(x))[, c("abbr", "layer")]))
    	}
    	else {
    		nrow(taxonomy(x))	
    	}
	}
)

#	dim is a primitive function
setMethod("dim",
    signature(x = "Vegsoup"),
	    function (x) {
			return(c(nrow(x), ncol(x)))
		}
)

if (!isGeneric("ncell")) {
setGeneric("ncell",
	function (x)
	standardGeneric("ncell"))
}
setMethod("ncell",
	signature(x = "Vegsoup"),
	function (x) {
    	prod(dim(x))
    }
)

#	matrix fill
#	used in summary
if (!isGeneric("fill")) {
setGeneric("fill",
	function (obj)
	standardGeneric("fill"))
}

setMethod("fill",
    signature(obj = "Vegsoup"),
    function (obj) {
		res <- sum(as.logical(obj) == 0) / prod(dim(obj))
		res <- (1 - res) * 100
		#	single plot object
		if (nrow(obj) == 1) {
			res <- 100
		}
		return(res)
	}
)