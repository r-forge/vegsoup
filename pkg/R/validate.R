#	Partition Analysis
#	to do: check dist slot
#	to do: implement ?testpart()
setGeneric("Partana",
	function (obj, ...)
		standardGeneric("Partana")
)

setMethod("Partana",
    signature(obj = "VegsoupPartition"),
    function (obj, verbose = FALSE, ...) {
		require(optpart)
		if (getK(obj) == 1)	stop("meaningless with k = ", getK(obj))
    	Xd <- as.dist(obj, ...)   	

		cpu.time <- system.time({		
			res <- optpart::partana(c = Partitioning(obj), dist = Xd)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n within-cluster to among-cluster similarity ratio:",
				round(res$ratio, 1))		
		}					
		return((res)) #invisible   	
    }
)

#	table deviance
setGeneric("Tabdev",
	function (obj, ...)
		standardGeneric("Tabdev")
)

setMethod("Tabdev",
	signature(obj = "VegsoupPartition"),
	function (obj, numitr = 99, verbose = FALSE, ...) {
		require(optpart)

		if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))

		cpu.time <- system.time({
			res <- optpart::tabdev(as.matrix(obj, ...),
				Partitioning(obj), nitr = numitr, ...)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n total deviance:", res$totdev)	
			cat("\n number of iterations performed:", numitr)	
		}			
		return(res)
		#return((res$spcdev)) #invisible
	}
)
