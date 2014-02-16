#	optimise partitioning using Dave Roberts optsil procedure
#if (!isGeneric("Optsil"))) {
setGeneric("Optsil",
	function (obj, maxitr = 100, verbose = FALSE, ...)
		standardGeneric("Optsil")
)
#}
setMethod("Optsil",
    signature(obj = "VegsoupPartition"),
    function (obj, maxitr = 100, verbose = FALSE, ...) {
		#	Imports:
		require(optpart)
		if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))
    	
    	nam <- names(obj@part) # save names    	
    	cl <- match.call()
    	    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}   	  	
    		
		cpu.time <- system.time({
			tmp <- optpart:::optsil(
					x = Partitioning(obj), dist = as.dist(obj, ...),
					maxitr = maxitr)
			obj@part <- as.integer(tmp$clustering)
			numitr <- tmp$numitr			
		})

		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}	
	
		names(obj@part) <- nam
		return(obj)
	}
)

#	optimise partitioning using Dave Roberts optindval procedure
#if (!isGeneric("Optindval"))) {
setGeneric("Optindval",
	function (obj, maxitr = 100, minsiz = 5, verbose = FALSE, ...)
		standardGeneric("Optindval")
)
#}
setMethod("Optindval",
    signature(obj = "VegsoupPartition"),
    function (obj, maxitr = 100, minsiz = 5, verbose = FALSE, ...) {
		#	Imports:
		#	require(optpart)
		
    	if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))

    	nam <- names(obj@part) # save names		
		cl <- match.call()
		
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}

		cpu.time <- system.time({
			tmp <- optpart::optindval(
					as.matrix(obj, ...), Partitioning(obj),
					maxitr = maxitr,
					minsiz = minsiz)
			obj@part <- as.integer(tmp$clustering)		
			numitr <- tmp$numitr		
		})
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}					
		names(obj@part) <- nam
		return(obj)
    }
)