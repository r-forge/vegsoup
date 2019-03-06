setGeneric("private",
	function (obj)
		standardGeneric("private")
)

setMethod("private",
	signature(obj = "VegsoupPartition"),
	function (obj) {
		r <- contingency(obj)
		r <- r > 0
		r[ rowSums(r > 0) != 1, ] <- FALSE
		return(r)
	}
)	
