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
	function (object, table = "constancy", ...) {
	r <- as.dendrogram(as.hclust(object, table = table, ...))
}
)