setGeneric("constancy",
	function (obj, percentage = TRUE, ...)
		standardGeneric("constancy")
)

setMethod("constancy",
	signature(obj = "Vegsoup"),
	function (obj, percentage = TRUE, ...) {
		r <- contingency(obj) / nrow(obj)
		if (percentage) {
			r <- round(r * 100, ...)
		}
		return(r)
	}
)

setMethod("constancy",
	signature(obj = "VegsoupPartition"),
	function (obj, percentage = TRUE, ...) {
		r1 <- contingency(obj)
		r2 <- matrix(partitions(obj),
			nrow = ncol(obj), ncol = getK(obj), byrow = TRUE)
		r <- r1 / r2
		if (percentage) {
			r <- round(r * 100, ...)
		}
		return(r)
	}
)