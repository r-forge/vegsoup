#if (!isGeneric("colSums")) {
setGeneric("colSums",
	function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("colSums"))
#}
setMethod("colSums",
	signature(x = "Vegsoup"),
	function (x, na.rm = FALSE, dims = 1, typeof = "logical", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	colSums(as.matrix(x, typeof = typeof), ...)
    }
)

#if (!isGeneric("rowSums")) {
setGeneric("rowSums",
	function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("rowSums"))
#}
setMethod("rowSums",
	signature(x = "Vegsoup"),
	function (x, na.rm = FALSE, dims = 1, typeof = "logical", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}	
    	rowSums(as.matrix(x, typeof = typeof), ...)
    }
)

#if (!isGeneric("rowMeans")) {
setGeneric("rowMeans", function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("rowMeans"))
#}
setMethod("rowMeans",
	signature(x = "Vegsoup"),
	function (x, na.rm = FALSE, dims = 1, typeof = "numeric", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	rowMeans(as.matrix(x, typeof = typeof), ...)
    }
)

#if (!isGeneric("colMeans")) {
setGeneric("colMeans", function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("colMeans"))
#}
setMethod("colMeans",
	signature(x = "Vegsoup"),
	function (x, na.rm = FALSE, dims = 1, typeof = "numeric", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	colMeans(as.matrix(x, typeof = typeof), ...)
    }
)