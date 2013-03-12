#if (!isGeneric("Sites")) {
setGeneric("Sites",
	function (obj)
		standardGeneric("Sites")
)
#}
setMethod("Sites",
    signature(obj = "Vegsoup"),
    function (obj) obj@sites
)
#if (!isGeneric("Sites<-")) {
setGeneric("Sites<-",
	function (obj, value)
		standardGeneric("Sites<-")
)
#}

#	Sites<-
setReplaceMethod("Sites",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		sel <- match(rownames(obj), rownames(value))
		if (any(is.na(sel))) {
			stop("non matching rownames not allowed")
		}
		obj <- obj[sel, ]
		obj@sites <- value				
		return(obj)		
	}
)
