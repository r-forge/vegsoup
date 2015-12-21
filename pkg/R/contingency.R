setGeneric("contingency",
	function (obj, ...)
		standardGeneric("contingency")
)

setMethod("contingency",
	signature(obj = "Vegsoup"),
	function (obj, ...) {
		cl <- match.call()

		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("argument mode not yet implemented for objects of class Vegsoup")
			}
			else {
				stop("omit mode argument for standard behaviour")
			}
		}
		else {
			r <- as.matrix(colSums(obj))
			colnames(r) <- 0
		}
		return(r)
	}
)


setMethod("contingency",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		cl <- match.call()

		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				r <- t(aggregate(as.logical(obj, ...),
					by = list(splitAbbr(obj)$layer),
					FUN = sum)[, -1])
				colnames(r) <- layers(obj)
			}
			else {
				stop("omit mode argument for standard behaviour")
			}
		}
		else {
			r <- aggregate(as.logical(obj), by = list(partitioning(obj)), FUN = sum)
			k <- r[, 1]
			r <- t(r[, -1])
			colnames(r) <- k
		}
		return(r)
	}
)
