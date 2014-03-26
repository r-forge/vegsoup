#	number of partitions
setGeneric("getK",
	function (x)
		standardGeneric("getK")
)
setMethod("getK",
	signature(x = "VegsoupPartition"),
	function (x) {
		x@k
	}	
)

#	retrieve partitions
if (!isGeneric("Partitioning")) {
setGeneric("Partitioning",
	function (x)
		standardGeneric("Partitioning")
)
}

setMethod("Partitioning",
	signature(x = "VegsoupPartition"),
	function (x) x@part
)

#	replace partitions
if (!isGeneric("Partitioning<-")) {
setGeneric("Partitioning<-",
	function (x, value)
		standardGeneric("Partitioning<-")
)
}

setReplaceMethod("Partitioning",
	signature(x = "VegsoupPartition", value = "numeric"),
	function (x, value) {
		if (length(value) != length(Partitioning(x))) {
			stop("replacement does not match in length", call. = FALSE)
		}
		if (is.null(names(value))) {		
			names(value) <- rownames(x)
		}
		else {
			if (length(intersect(names(value), rownames(x))) != nrow(x)) {
				stop("if value has names, those have to match rownames(x)")
			}
			else {
				value <- value[match(rownames(x), names(value))]
			}
		}
		x@part <- value
		x@k <- length(unique(value))		
		return(x)		
	}
)

#	subset by partiton
if (!isGeneric("Partition")) {
setGeneric("Partition",
	function (x, value, ...)
		standardGeneric("Partition")
)
}

setMethod("Partition",
	signature(x = "VegsoupPartition"),
	function (x, value) {
			stopifnot(!any(value > getK(x)))	
			x[x@part %in% value, ]
	}		
)

#	tabulate partition vector to matrix
if (!isGeneric("PartitioningMatrix")) {
setGeneric("PartitioningMatrix",
	function (x)
		standardGeneric("PartitioningMatrix")
)
}

setMethod("PartitioningMatrix",
    signature(x = "VegsoupPartition"),
	function (x) {
		res <- t(sapply(Partitioning(x),
			function (y) {
				as.numeric(y == levels(factor(Partitioning(x))))
			}))
		dimnames(res)[2] <- list(levels(factor(Partitioning(x))))
    return(res)                                                                                                                              
	}
)

#	matrix of possible partition combinations
setGeneric("PartitioningCombinations",
	function (x, collapse)
		standardGeneric("PartitioningCombinations")
)

.PartitioningCombinations <- function (x, collapse) {	
	if (missing(collapse)) {
		collapse = "+"
	}	
	cluster <- levels(as.factor(Partitioning(x)))	
	cl.comb <- function (x) {
		k <- length(x) #getK(x)# 
		ep <- diag(1, k, k)
		names.ep <- x
	    for (j in 2:k) {
	    	nco <- choose(k, j)
	    	co <- combn(k, j)
	    	epn <- matrix(0, ncol = nco, nrow = k)
			for (i in 1:ncol(co)) {
				epn[co[, i], i] <- 1
				names.ep <- c(names.ep, paste(x[co[,i]], collapse = collapse))
			}
		ep <- cbind(ep, epn)
		}
		colnames(ep) <- names.ep
		return(ep)
	}
	res <- cl.comb(cluster)
	return(res)
}

setMethod("PartitioningCombinations",
	signature(x = "VegsoupPartition"),
	.PartitioningCombinations
)