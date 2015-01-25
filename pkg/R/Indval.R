setGeneric("Indval",
	function (obj, ...)
		standardGeneric("Indval")
)

setMethod("Indval",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		res <- labdsv::indval(as.logical(obj), partitioning(obj), ...)$indval
		return(res)
	}
)
