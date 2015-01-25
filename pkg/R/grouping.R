#	get predefined grouping vector
setGeneric("apriori",
	function (obj)
		standardGeneric("apriori")
)
setMethod("apriori",
	signature(obj = "Vegsoup"),
	function (obj) obj@group
)