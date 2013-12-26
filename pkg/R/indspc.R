#	compositional indicator species analysis
#	to do: documentation
#if (!isGeneric("Indspc")) {	
setGeneric("Indspc",
	function (obj, ...)
		standardGeneric("Indspc")
)
#}
setMethod("Indspc",
    signature(obj = "Vegsoup"),
    function (obj, method, ...) {
    	#	Suggests:
    	require(labdsv)
    	res <- indspc(as.logical(obj), dis = as.dist(obj), ...)
		return(res)    	
    }
)