#	dissimilarity diameters
if (!isGeneric("disdiam")) {
setGeneric("disdiam",
	function (x, ...)
		standardGeneric("disdiam")
)
}

setMethod("disdiam",
    signature(x = "VegsoupPartition"),
    function (x, ...) {
    	#	Imports
    	#	require(optpart)
		if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))    	
		r <- optpart::disdiam(Partitioning(x), as.dist(x, ...))
		return(r)    	
    }
)