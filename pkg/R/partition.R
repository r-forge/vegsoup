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
if (!isGeneric("partitioning")) {
setGeneric("partitioning",
	function (x)
		standardGeneric("partitioning")
)
}

setMethod("partitioning",
	signature(x = "VegsoupPartition"),
	function (x) x@part
)

#	replace partitions
if (!isGeneric("partitioning<-")) {
setGeneric("partitioning<-",
	function (x, value)
		standardGeneric("partitioning<-")
)
}

setReplaceMethod("partitioning",
	signature(x = "VegsoupPartition", value = "numeric"),
	function (x, value) {
		if (length(value) != length(partitioning(x))) {
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
if (!isGeneric("partition")) {
setGeneric("partition",
	function (x, value, ...)
		standardGeneric("partition")
)
}

setMethod("partition",
	signature(x = "VegsoupPartition"),
	function (x, value) {
			stopifnot(!any(value > getK(x)))
			x[x@part %in% value, ]
	}
)

#	tabulate partition vector to matrix
if (!isGeneric("partitioningMatrix")) {
setGeneric("partitioningMatrix",
	function (x)
		standardGeneric("partitioningMatrix")
)
}

setMethod("partitioningMatrix",
	signature(x = "VegsoupPartition"),
	function (x) {
		res <- t(sapply(partitioning(x),
			function (y) {
				as.numeric(y == levels(factor(partitioning(x))))
			}))
		dimnames(res)[2] <- list(levels(factor(partitioning(x))))
	return(res)
	}
)

#	matrix of possible partition combinations
setGeneric("partitioningCombinations",
	function (x, collapse)
		standardGeneric("partitioningCombinations")
)

.partitioningCombinations <- function (x, collapse) {
	if (missing(collapse)) {
		collapse = "+"
	}	
	cluster <- levels(as.factor(partitioning(x)))
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

setMethod("partitioningCombinations",
	signature(x = "VegsoupPartition"),
	.partitioningCombinations
)