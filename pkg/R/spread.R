#	list occurences of species in partitions
setGeneric("spread",
	function (obj)
		standardGeneric("spread")
)

setMethod("spread",
	signature(obj = "VegsoupPartition"),
	function (obj) {
	part  <- partitioning(obj)
	X <- as.logical(obj)
	
	if (getK(obj) == 1) {
		stop("single partition is not meaningful for spread", call. = FALSE)
	}
	
	res <- apply(X, 2, function (y) {
		sapply(rownames(X[y > 0, , drop = FALSE]),
			function (z) {
				part[which(names(part) == z)]
			},
			USE.NAMES = FALSE)
		}			
	)
	return(res)
	}
)