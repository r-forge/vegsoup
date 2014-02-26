#	optimise partitioning using Dave Roberts optsil procedure
#	optpart defines: function (x, dist, maxitr, ...)
#if (!isGeneric("optsil"))) {
setGeneric("optsil",
	function (x, maxitr = 100, verbose = FALSE, ...)
		standardGeneric("optsil")
)
#}

setMethod("optsil",
    signature(x = "VegsoupPartition"),
    function (x, maxitr = 100, verbose = FALSE, ...) {
		#	Imports: optpart
		#	require(optpart)
		
		if (getK(x) == 1) stop("meaningless with k = ", getK(x))
    	
    	nam <- names(x@part) # save names    	
    	cl <- match.call()
    	    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}   	  	
    		
		cpu.time <- system.time({
			tmp <- optpart::optsil(
					x = Partitioning(x), dist = as.dist(x), #, ...
					maxitr = maxitr)
			x@part <- as.integer(tmp$clustering)
			numitr <- tmp$numitr			
		})

		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", getK(x), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}	
	
		names(x@part) <- nam
		return(x)
	}
)

#	optimise partitioning using Dave Roberts optindval procedure
#	optpart defines: function (veg,clustering,maxitr=100,minsiz=5, ...) 
#if (!isGeneric("optindval"))) {
setGeneric("optindval",
	function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...)
		standardGeneric("optindval")
)
#}
setMethod("optindval",
    signature(x = "VegsoupPartition"),
    function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...) {
		#	Imports: optpart
		#	require(optpart)
		
    	if (getK(x) == 1) stop("meaningless with k = ", getK(x))

    	nam <- names(x@part) # save names		
		cl <- match.call()
		
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}

		cpu.time <- system.time({
			tmp <- optpart::optindval(
					as.matrix(x, ...), Partitioning(x),
					maxitr = maxitr,
					minsiz = minsiz)
			x@part <- as.integer(tmp$clustering)		
			numitr <- tmp$numitr		
		})
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", getK(x), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}					
		names(x@part) <- nam
		return(x)
    }
)