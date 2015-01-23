#if (!isGeneric("sites")) {
setGeneric("sites",
	function (obj, ...)
		standardGeneric("sites")
)
#}
setMethod("sites",
    signature(obj = "Vegsoup"),
    function (obj) obj@sites
)
#if (!isGeneric("sites<-")) {
setGeneric("sites<-",
	function (obj, value)
		standardGeneric("sites<-")
)
#}

#	sites<-
setReplaceMethod("sites",
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
