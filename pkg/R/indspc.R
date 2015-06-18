#	compositional indicator species analysis
#	to do: documentation
#if (!isGeneric("compspec")) {	
setGeneric("compspec",
	function (obj, ...)
		standardGeneric("compspec")
)
#}
setMethod("compspec",
	signature(obj = "Vegsoup"),
	function (obj, method, ...) {
		#	Suggests:
		require(labdsv)
		Xd <- as.dist(obj)
		#	ensure dissimilarities
		if (max(Xd) > 1) Xd <- Xd / max(Xd)
		res <- labdsv::compspec(as.logical(obj), dis = Xd, ...)
		return(res)		
	}
)