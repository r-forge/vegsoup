setGeneric("Constancy",
	function (obj, percentage = TRUE, ...)
		standardGeneric("Constancy")
)
setMethod("Constancy",
	signature(obj = "VegsoupPartition"),
	function (obj, percentage = TRUE, ...) {
		res1 <- Contingency(obj)
		res2 <- matrix(as.vector(table(Partitioning(obj))),
			nrow = ncol(obj), ncol = getK(obj), byrow = TRUE)		
		res <- res1 / res2
		if (percentage) {
			res <- round(res * 100, ...)
		}		
		return(res)
	}
)