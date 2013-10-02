setGeneric("contingency",
	function (obj, ...)
		standardGeneric("contingency")
)
setMethod("contingency",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		cl <- match.call()
    	    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
				res <- t(aggregate(as.logical(obj, ...),
					by = list(split.abbr(obj)$layer),
					FUN = sum)[, -1])
				colnames(res) <- Layers(obj)
			} else {
				stop("omit mode argument for standard behaviour")
			}
    	} else {  
			res <- t(aggregate(as.logical(obj),
				by = list(Partitioning(obj)), FUN = sum))[-1, ]
				colnames(res) <- unique(Partitioning(obj))
				rownames(res) <- colnames(obj)
		}	
		return(res)
	}	
)
