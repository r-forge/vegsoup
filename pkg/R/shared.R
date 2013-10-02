#	shared species
setGeneric("Shared",
	function (obj)
		standardGeneric("Shared")
)

setMethod("Shared",
	signature(obj = "VegsoupPartition"),
	function (obj) {
		X <- constancy(obj) > 0
		mode(X) <- "numeric"		
		res <- vegan::designdist(t(X), method = "J/(A+B)*100", terms = "binary")
	return(res)
	}
)
