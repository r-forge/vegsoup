setGeneric("isamic",
	function (x)
		standardGeneric("isamic")
)
setMethod("isamic",
	signature(x = "VegsoupPartition"),
	function (x) { 	
	   	tmp <- constancy(x) / 100
    	res <- apply(tmp, 1, function (x) {
    			2 * sum(abs(as.numeric(x) - 0.5)) / ncol(tmp)
    		})
    	return(res)
    }
)