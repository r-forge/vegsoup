#if (!isGeneric("Sites")) {
setGeneric("Sites",
	function (obj, ...)
		standardGeneric("Sites")
)
#}
setMethod("Sites",
    signature(obj = "Vegsoup"),
    function (obj) obj@sites
)
#if (!isGeneric("Sites<-")) {
setGeneric("Sites<-",
	function (obj, value, ...)
		standardGeneric("Sites<-")
)
#}


#	replacement method for sites
#	to do: needs comprehensive validity checks!
setReplaceMethod("Sites",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		obj@sites <- value		
		return(obj)		
	}
)
