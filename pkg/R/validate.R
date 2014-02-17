#	Partition Analysis
#	to do: check dist slot
#	to do: implement ?testpart()
setGeneric("partana",
	function (x, verbose = FALSE, ...)
		standardGeneric("partana")
)

setMethod("partana",
    signature(x = "VegsoupPartition"),
    function (x, verbose = FALSE, ...) {
    	#	Imports:
		require(optpart)
		if (getK(x) == 1)	stop("meaningless with k = ", getK(x))
    	Xd <- as.dist(x)# , ...)   	

		cpu.time <- system.time({		
			res <- optpart::partana(c = Partitioning(x), dist = Xd)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(x), "cells",
				"and", getK(x), "partitions:",
				cpu.time[3], "sec")
			cat("\n within-cluster to among-cluster similarity ratio:",
				round(res$ratio, 1))		
		}					
		return((res)) #invisible   	
    }
)

#	table deviance
setGeneric("tabdev",
	function (x, numitr = 99, verbose = FALSE, ...)
		standardGeneric("tabdev")
)

setMethod("tabdev",
	signature(x = "VegsoupPartition"),
	function (x, numitr = 99, verbose = FALSE, ...) {
		#	Imports:
		require(optpart)

		if (getK(x) == 1) stop("meaningless with k = ", getK(x))

		cpu.time <- system.time({
			res <- optpart::tabdev(as.matrix(x, ...),
				Partitioning(x), nitr = numitr, ...)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(x), "cells",
				"and", getK(x), "partitions:",
				cpu.time[3], "sec")
			cat("\n total deviance:", res$totdev)	
			cat("\n number of iterations performed:", numitr)	
		}			
		return(res)
		#return((res$spcdev)) #invisible
	}
)
