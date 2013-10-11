#	shared species
setGeneric("shared",
	function (x)
		standardGeneric("shared")
)

setMethod("shared",
	signature(x = "VegsoupPartition"),
	function (x) {
		X <- constancy(x) > 0
		mode(X) <- "numeric"		
		res <- vegan::designdist(t(X), method = "J/(A+B)*100", terms = "binary")
	return(res)
	}
)
