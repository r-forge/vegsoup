#	get predefined grouping vector
setGeneric("AprioriGrouping",
	function (obj)
		standardGeneric("AprioriGrouping")
)
setMethod("AprioriGrouping",
	signature(obj = "Vegsoup"),
	function (obj) obj@group
)