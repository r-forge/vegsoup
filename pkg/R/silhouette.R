setGeneric("silhouette",
	function (x, ...)
		standardGeneric("silhouette")
)

setMethod("silhouette",
	signature(x = "VegsoupPartition"),
	function (x, ...) {
		if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))
		
		res <- cluster::silhouette(partitioning(x), dist = as.dist(x, ...))
		rownames(res) <- rownames(x)
		return(res)
	}
)