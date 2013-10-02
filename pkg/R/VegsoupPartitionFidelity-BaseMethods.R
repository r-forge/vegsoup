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

