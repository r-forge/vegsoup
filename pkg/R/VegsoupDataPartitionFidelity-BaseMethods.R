setMethod("summary",
    signature(object = "VegsoupDataPartitionFidelity"),
	function (object, ...) {
	cat("method", object@method)
	if (all(is.na(object@lowerCI))) {
		cat("\nno bootstrap performed")	
	} else {
		cat("\nnumber of bootstrap replicates", object@nboot)
	}
}
)

setGeneric("getStat",
	function (obj, ...)
		standardGeneric("getStat")
)
setMethod("getStat",
	signature(obj = "VegsoupDataPartitionFidelity"),
	function (obj) obj@stat	
)

