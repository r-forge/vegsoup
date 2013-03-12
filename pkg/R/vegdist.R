#	dissimilarity
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist",
	function (obj, ...)
		standardGeneric("vegdist")
)
#}
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist<-",
	function (obj, value, ...)
		standardGeneric("vegdist<-")
)
#}

setMethod("vegdist",
	signature(obj = "Vegsoup"),
	function (obj, ...) {
		obj@dist
	}
)
setReplaceMethod("vegdist",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		#	from vegan::vegdist
		METHODS <- c("manhattan", "euclidean", "canberra", "bray",
		"kulczynski", "gower", "morisita", "horn", "mountford",
		"jaccard", "raup", "binomial", "chao", "altGower", "cao")
		method <- METHODS[pmatch(value, METHODS)]
    	
    	if (is.na(method)) 
        	stop("invalid distance method")
    	if (method == -1) 
        	stop("ambiguous distance method")
		obj@dist <- method

		return(obj)
	}
)

#	vegan: dis, toolong = 1, trace = TRUE		
#	connectedness of dissimilarities
#if (!isGeneric("decostand<-")) {
setGeneric("distconnected",
	function (obj, ...)
		standardGeneric("distconnected")
)
#}
setMethod("distconnected",
	signature(obj = "Vegsoup"),
	function (obj, ...) {
		vegan::distconnected(as.dist(obj), ...)
	}
)