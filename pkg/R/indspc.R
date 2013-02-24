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