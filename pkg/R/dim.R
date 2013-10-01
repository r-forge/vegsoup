#if (!isGeneric("nrow")) {
setGeneric("nrow", function(x)
	standardGeneric("nrow"))
#}
setMethod("nrow",
    signature(x = "Vegsoup"),
    function (x) {
		nrow(Sites(x))
#		length(rownames(x))
	}
)
#if (!isGeneric("dim")) {
setGeneric("ncol", function (x)
	standardGeneric("ncol"))
#}
setMethod("ncol",
    signature(x = "Vegsoup"),
    function (x) {
    	if (length(Layers(x)) > 1) {
    		nrow(unique(Species(x)[, 2:3])) # c("abbr", "layer")
    	}
    	else {
    		nrow(Taxonomy(x))	
    	}
	}
)
#	'dim' is a primitive function
setMethod("dim",
    signature(x = "Vegsoup"),
	    function (x) {
			c(nrow(x), ncol(x))
		}
)
#if (!isGeneric("ncell")) {
setGeneric("ncell",
	function (x)
	standardGeneric("ncell"))
#}
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
		#x <- nrow(obj)
		#y <- sum(dim(obj))
		#res <- x/y * 100
		#	zeros <- 
		res <- sum(as.logical(obj) == 0) / prod(dim(obj))
		res <- (1 - res) * 100
		#	single plot object
		if (nrow(obj) == 1) {
			res <- 100
		}
		return(res)
	}
)