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
		k <- getK(x)
		if (k == 1) stop("meaningless with k = ", k)
		
		n <- names(x@part) # save names
		cl <- match.call() # catch call, not used at the moment
		
		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		
		cpu.time <- system.time( {
			r <- optpart::optsil(
					x = partitioning(x), dist = as.dist(x), #, ...
					maxitr = maxitr)
			i <- r$numitr # iterations
		} )
		
		#	modify object
		x@part <- as.integer(r$clustering)
		names(x@part) <- n
		
		#	test if have to change slot k
		kk <- length(unique(partitioning(x)))
		if (k != kk) {
			x@k <- kk
			#	good language
			message(ifelse(k > kk, "decreased ", "increased "),
				"number of partitons from ", k, " to ", kk)
		}
		
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", k, "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", i)
		}
		return(x)
	}
)

#	optimise partitioning using Dave Roberts optindval procedure
#	optpart defines: function (veg, clustering, maxitr = 100, minsiz = 5, ...) 
#if (!isGeneric("optindval"))) {
setGeneric("optindval",
	function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...)
		standardGeneric("optindval")
)
#}
setMethod("optindval",
	signature(x = "VegsoupPartition"),
	function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...) {
		k <- getK(x)
		if (k == 1) stop("meaningless with k = ", k)
		
		n <- names(x@part) # save names
		cl <- match.call() # catch call, not used at the moment
		
		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		
		cpu.time <- system.time( {
			r <- optpart::optindval(
					as.matrix(x, ...), partitioning(x),
					maxitr = maxitr,
					minsiz = minsiz)
			i <- r$numitr # iterations
		} )
		
		#	modify object
		x@part <- as.integer(r$clustering)
		names(x@part) <- n
		
		#	test if have to change slot k
		kk <- length(unique(partitioning(x)))
		if (k != kk) {
			x@k <- kk
			#	good language
			message(ifelse(k > kk, "decreased ", "increased "),
				"number of partitons from ", k, " to ", kk)
		}
		
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", k, "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", i)
		}
		return(x)
	}
)
