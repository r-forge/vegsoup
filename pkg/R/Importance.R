setGeneric("Importance",
	function (obj, ...)
		standardGeneric("Importance")
)

setMethod("Importance",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		cl <- match.call()
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
				res <- t(aggregate(as.matrix(obj, ...),
					by = list(split.abbr(obj)$layer),
					FUN = sum)[, -1])
				res <- res / Contingency(obj, ...)
				#	division by zero
				res[is.na(res)] <- 0
				colnames(res) <- Layers(obj)									
			} else {
				stop("omit mode argument for standard behaviour")					
			}
    	} else {  		
			res <- t(aggregate(as.matrix(obj, ...),
				by = list(Partitioning(obj)), FUN = sum))[-1, ]
			res <- res / Contingency(obj)
			#	division by zero
			res[is.na(res)] <- 0
			colnames(res) <- unique(Partitioning(obj))
			rownames(res) <- colnames(obj)
		}
		return(res)
	}
)