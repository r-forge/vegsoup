#	standardisation
#	vegan defines:
#	decostand(x, method, MARGIN, range.global, logbase = 2,
#	na.rm = FALSE, ...)

#if (!isGeneric("decostand")) {
setGeneric("decostand",
	function (x, method, MARGIN, range.global, logbase = 2, na.rm = FALSE, ...)
	standardGeneric("decostand"))
#}

#if (!isGeneric("decostand<-")) {
setGeneric("decostand<-",
	function (x, value)
		standardGeneric("decostand<-")
)
#}

setMethod("decostand",
		signature(x = "Vegsoup"),
	    	function (x) {
				slot(slot(x, "decostand"), "method")
			}
)

setReplaceMethod("decostand",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		#	taken from vegan
	    METHODS <- c("total", "max", "frequency", "normalize", "range", 
            "standardize", "pa", "chi.square", "hellinger", "log",
            "wisconsin")            
        value <- match.arg(value, METHODS, several.ok = TRUE)		
		x@decostand <- new("decostand", method = value)
		return(x)		
	}
)

setReplaceMethod("decostand",
	signature(x = "VegsoupPartition", value = "character"),
	function (x, value) {
		#	taken from vegan
	    METHODS <- c("total", "max", "frequency", "normalize", "range", 
            "standardize", "pa", "chi.square", "hellinger", "log",
            "wisconsin")            
        value <- match.arg(value, METHODS, several.ok = TRUE)
 		
 		if (is.null(decostand(i.prt))) {
 			x@decostand <- new("decostand", method = value)	
 		}
 		else {	
	 		if (value != decostand(x)) {	
				x@decostand <- new("decostand", method = value)		
				#	recompute
				x <- VegsoupPartition(x, k = getK(x), method = x@method)
				message("recomputed partitoning")
			}
		}	   		
		return(x)		
	}
)

setReplaceMethod("decostand",
	signature(x = "Vegsoup", value = "NULL"),
 	function (x, value) {
		x@decostand <- new("decostand", method = NULL)
		return(x)
	}
)

setReplaceMethod("decostand",
	signature(x = "VegsoupPartition", value = "NULL"),
 	function (x, value) {
 		  if (!is.null(decostand(x))) {	
			x@decostand <- new("decostand", method = NULL)		
			#	recompute
			x <- VegsoupPartition(x, k = getK(x))
			message("recomputed partitoning")
		}
		return(x)
	}
)