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

#	plotting method hist
setMethod("hist",
	signature(x = "VegsoupDataPartitionFidelity"),
	function (x, ...) {
		fig <- hist(x@stat, xlab = x@method, ...)
		return(invisible(fig))
	}

)

