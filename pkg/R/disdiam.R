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
		if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))
		r <- optpart::disdiam(partitioning(x), as.dist(x))
		return(r)
	}
)
