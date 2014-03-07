#	Dufrene & Legendre's indicator value
#	to do: documentation
#	See also \code{\link{Fidelity}}, \code{\link{Phi}} and \code{\link{FisherTest}}
setGeneric("Indval",
	function (obj, ...)
		standardGeneric("Indval")
) 
setMethod("Indval",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
		#require(labdsv)
		res <- labdsv::indval(as.logical(obj), Partitioning(obj), ...)$indval
		return(res)
	}
)
