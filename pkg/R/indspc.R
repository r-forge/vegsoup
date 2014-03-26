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
    	Xd <- as.dist(obj)
    	#	ensure dissimilarities
    	if (max(Xd) > 1) Xd <- Xd / max(Xd)
    	res <- indspc(as.logical(obj), dis = Xd, ...)
		return(res)    	
    }
)