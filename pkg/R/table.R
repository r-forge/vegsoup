setGeneric("Contingency",
	function (obj, ...)
		standardGeneric("Contingency")
)
setMethod("Contingency",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		cl <- match.call()
    	    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
				res <- t(aggregate(as.logical(obj, ...),
					by = list(DecomposeNames(obj)$layer),
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

setGeneric("Constancy",
	function (obj, ...)
		standardGeneric("Constancy")
)
setMethod("Constancy",
	signature(obj = "VegsoupPartition"),
	function (obj, percentage = TRUE, ...) {
		res1 <- Contingency(obj)
		res2 <- matrix(as.vector(table(Partitioning(obj))),
			nrow = ncol(obj), ncol = getK(obj), byrow = TRUE)		
		res <- res1 / res2
		if (percentage) {
			res <- round(res * 100, ...)
		}		
		return(res)
	}
)

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
					by = list(DecomposeNames(obj)$layer),
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