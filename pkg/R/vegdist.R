#	dissimilarity

#	vegan defines:
#	vegdist(x, method="bray", binary=FALSE, diag=FALSE,
#	upper=FALSE, na.rm = FALSE, ...)

setGeneric("vegdist",
	function (x, method = "bray", binary = FALSE, diag = FALSE,
	upper=FALSE, na.rm = FALSE, ...)
		standardGeneric("vegdist")
)

#}
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist<-",
	function (x, value, ...)
		standardGeneric("vegdist<-")
)
#}

setMethod("vegdist",
	signature(x = "Vegsoup"),
	function (x, ...) {
		x@dist
	}
)
setReplaceMethod("vegdist",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		#	from vegan::vegdist
		METHODS <- c("manhattan", "euclidean", "canberra", "bray",
		"kulczynski", "gower", "morisita", "horn", "mountford",
		"jaccard", "raup", "binomial", "chao", "altGower", "cao")
		method <- METHODS[pmatch(value, METHODS)]
    	
    	if (is.na(method)) 
        	stop("invalid distance method")
    	if (method == -1) 
        	stop("ambiguous distance method")
		x@dist <- method

		return(x)
	}
)

#	vegan defines:
#   distconnected(dis, toolong = 1, trace = TRUE)		
#	connectedness of dissimilarities
#if (!isGeneric("distconnected<-")) {
setGeneric("distconnected",
	function (dis, toolong = 1, trace = TRUE)
		standardGeneric("distconnected")
)
#}
setMethod("distconnected",
	signature(dis = "Vegsoup"),
	function (dis, toolong = 1, trace = TRUE) {
		vegan::distconnected(as.dist(obj), ...)
	}
)