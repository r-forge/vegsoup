setGeneric("reorder")

".reorder.VegsoupPartition" <- function (x, dendrogram, ...) {
	#	get order of leafs in dendrogram
	d  <- as.dendrogram(dendrogram)
	d <- cut(d, 0)$lower
	d <- sapply(d, function (x) dendrapply(x, function (n) attr(n, "label")))
	#	partitioning as character
	p <- factor(partitioning(x))
	levels(p) <- as.character(order(d))
	
	p <- structure(as.numeric(as.character(p)), names = names(p))
	x@part <- p	
	
	return(x)
}

#	reorder to dendrogram
setMethod("reorder",
	signature(x = "VegsoupPartition"),
	.reorder.VegsoupPartition
)	
