#	Indicator species analysis by Murdoch preference function
#	to do: documentation
setGeneric("murdoch",
	function (x, ...)
		standardGeneric("murdoch")
)
setMethod("murdoch",
    signature(x = "VegsoupPartition"),
    function (x, minplt, type, ...) {
    	#	Imports:
    	#	require(optpart)
    	if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))
    	if (missing(minplt))
    		minplt <- 1
		if (missing(type))
			part <- Partitioning(x)
			
		if (any(table(part) == 1)) {
			stop("singleton present")
		}	
		ks <- getK(x)
		res <- matrix(0, nrow = dim(x)[2], ncol = ks)
		dimnames(res) <- list(colnames(x), 1:ks)
		pval <- matrix(0, nrow = dim(x)[2], ncol = ks)
		dimnames(pval) <- list(colnames(x), 1:ks)
		res.ls <- vector("list", length = ks)
		names(res.ls) <- 1:ks
		for (i in 1:ks) {
			res.ls[[i]] <- optpart::murdoch(as.logical(x),
				part == i, minplt = minplt)
			res[,i] <- res.ls[[i]]$murdoch
			pval[,i] <- round(res.ls[[i]]$pval, 3)
		}
    	return(c(res.ls, list(murdoch = res, pval = pval)))
    }
)