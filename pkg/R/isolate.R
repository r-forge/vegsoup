#if (!isGeneric("isolate")) {
setGeneric("isolate",
	function (x, ...)
		standardGeneric("isolate")
)
#}

setMethod("isolate",
	signature(x = "VegsoupPartition"),
	function (x, plot, ...) {
		if (is.character(plot)) {
			i <- match(plot, rownames(x))
			test <- any(is.na(i))
			if (test) {
				stop("plot identifier ", plot[is.na(i)], " not found", call. = FALSE)
			}
			k <- getK(x) + 1
			x@part[ i ] <- k
			x@k <- k
		}		
	return(x)
	}
)		