
#	standardisation
#if (!isGeneric("decostand")) {
setGeneric("decostand", function (obj)
	standardGeneric("decostand"))
#}
#if (!isGeneric("decostand<-")) {
setGeneric("decostand<-",
	function (obj, value)
		standardGeneric("decostand<-")
)
#}
setMethod("decostand",
		signature(obj = "Vegsoup"),
	    	function (obj) {
				slot(slot(obj, "decostand"), "method")
			}
)
setReplaceMethod("decostand",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		#	taken from vegan
	    METHODS <- c("total", "max", "frequency", "normalize", "range", 
            "standardize", "pa", "chi.square", "hellinger", "log",
            "wisconsin")            
        value <- match.arg(value, METHODS, several.ok = TRUE)		
		value <- new("decostand", method = value)
		obj@decostand <- value		
		return(obj)		
	}
)
setReplaceMethod("decostand",
	signature(obj = "Vegsoup", value = "NULL"),
 	function (obj, value) {
		obj@decostand <- new("decostand", method = NULL)
		return(obj)
	}
)