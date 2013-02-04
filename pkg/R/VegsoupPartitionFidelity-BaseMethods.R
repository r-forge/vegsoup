setMethod("summary",
    signature(object = "VegsoupPartitionFidelity"),
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
	signature(obj = "VegsoupPartitionFidelity"),
	function (obj) obj@stat	
)

#	plotting method hist
setMethod("hist",
	signature(x = "VegsoupPartitionFidelity"),
	function (x, ...) {
		fig <- hist(x@stat, xlab = x@method, ...)
		return(invisible(fig))
	}

)

