#	congruence between indicator and target species.
#	Halme's indicator power
#	to do: documentation
#if (!isGeneric("Indpower")) {			
setGeneric("Indpower",
	function (obj, ...)
		standardGeneric("Indpower")
)
#}
setMethod("Indpower",
    signature(obj = "Vegsoup"),
    function (obj, ...) {
    	res <- indpower(as.logical(obj), ...)
    	diag(res)  <- NA
    	if (type == 0)
			res <- rowMeans(res, na.rm = TRUE)
		return(res)    	
    }
)


#	compositional indicator species analysis
#	to do: documentation
#if (!isGeneric("Indspc")) {	
setGeneric("Indspc",
	function (obj, ...)
		standardGeneric("Indspc")
)
#}
setMethod("Indspc",
    signature(obj = "Vegsoup"),
    function (obj, method, ...) {
    	if (inherits(obj, "VegsoupPartition")) {
    		dis <- vegdist(as.logical(obj), obj@dist)
    	} else {
   			if (missing(method)) {    			
				dis <- vegdist(as.logical(obj), "bray")
    		} else {
    			dis <- vegdist(as.logical(obj), ...)
    		}    		
  	 	}
    	res <- indspc(as.logical(obj), dis = dis, ...)
		return(res)    	
    }
)





### start delete
#setGeneric("AbundanceScale",
#	function (obj)
#		standardGeneric("AbundanceScale")
#)

#setGeneric("AbundanceScale<-",
#	function (obj, value)
#		standardGeneric("AbundanceScale<-")
#)

#setMethod("AbundanceScale",
#   signature(obj = "Vegsoup"),
#    function (obj) obj@coverscale
#)

#setReplaceMethod("AbundanceScale",
#	signature(obj = "Vegsoup", value = "list"),
#	function (obj, value) {
#		#	to do: needs checking of list structure!
#		#	to do: needs checking of species slots!
#		warning("use coverscale")
#		return(obj)		
#	}
#)
### end delete
