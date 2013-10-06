setGeneric("getStat",
	function (obj, ...)
		standardGeneric("getStat")
)
setMethod("getStat",
	signature(obj = "VegsoupPartitionFidelity"),
	function (obj) obj@stat	
)