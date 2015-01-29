setGeneric("as.hclust",
	function (x, ...)
		standardGeneric("as.hclust")
)

setMethod("as.hclust",
	signature(x = "VegsoupPartition"),
	function (x, table = "constancy", ...) {
		switch(table,
			   constancy = {
			hclust(vegdist(t(constancy(x)), vegdist(x)), ...)
			   }, contingency = {
			hclust(vegdist(t(contingency(x)), vegdist(x)), ...)
			   }, average = {
			hclust(vegdist(t(average(x)), vegdist(x)), ...)
			   })
}
)

setGeneric("as.dendrogram",
	function (object, ...)
		standardGeneric("as.dendrogram")
)

setMethod("as.dendrogram",
	signature(object = "VegsoupPartition"),
	function (object, table = "constancy", labels = NULL, ...) {
	r <- as.dendrogram(as.hclust(object, table = table, ...))
	
	if (!is.null(labels)) {
		ll <- unique(cbind(partitioning(object), sites(object)[labels]))
		leaf <- function (n) {
			if (is.leaf(n)) {
				i <- match(as.numeric(attr(n, "label")), ll[,1 ])
				attr(n, "label") <- as.character(ll[, 2][i])
			}
			return(n)
			}
	r <- dendrapply(r, leaf)
	}	
	return(r)	
}
)