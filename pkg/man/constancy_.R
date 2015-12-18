setGeneric("constancy",
	function (obj, percentage = TRUE, ...)
		standardGeneric("constancy")
)
setMethod("constancy",
	signature(obj = "VegsoupPartition"),
	function (obj, percentage = TRUE, ...) {
		res1 <- contingency(obj)
		res2 <- matrix(as.vector(table(partitioning(obj))),
			nrow = ncol(obj), ncol = getK(obj), byrow = TRUE)
		res <- res1 / res2
		if (percentage) {
			res <- round(res * 100, ...)
		}
		return(res)
	}
)