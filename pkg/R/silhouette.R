setGeneric("silhouette",
	function (x, ...)
		standardGeneric("silhouette")
)

setMethod("silhouette",
    signature(x = "VegsoupPartition"),
    function (x, ...) {
    	#	Imports:
    	#	require(cluster)
		if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))
    	
		res <- cluster::silhouette(Partitioning(x), dist = as.dist(x, ...))
		return(res)    	
    }
)