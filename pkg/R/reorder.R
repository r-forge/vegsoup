setGeneric("reorder")

".reorder.VegsoupPartition" <- function (x, dendrogram, ...) {
	#	get order of leafs in dendrogram	
	if (!inherits(dendrogram, "dendrogram")) {
		d  <- as.dendrogram(dendrogram)		
	} else {
		d <- dendrogram
	}

	d <- cut(d, 0)$lower
	d <- sapply(d, function (x) dendrapply(x, function (n) attr(n, "label")))
	ll <- data.frame(x = as.numeric(names(partitions(x))),
		d = as.numeric(d), xd = NA, stringsAsFactors = FALSE)

	for (i in ll$d) {
		ll$xd[ i ] <- which(i == d)
	}
	
	p <- factor(partitioning(x), levels = ll$x, labels = ll$xd)	
	p <- structure(as.numeric(as.character(p)), names = names(p))
	x@part <- p	
	
	return(x)
}

#	reorder to dendrogram
setMethod("reorder",
	signature(x = "VegsoupPartition"),
	.reorder.VegsoupPartition
)	
