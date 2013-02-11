#	dissimilarity
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist",
	function (x, ...)
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
		x@dist <- method
		x	
	}
)	
#	retrieve distance matrix
#	to do: documentation
setGeneric("as.dist",
	function (m, diag = FALSE, upper = FALSE, ...)
		standardGeneric("as.dist")
)
setMethod("as.dist",
	signature(m = "Vegsoup"),
	function (m, binary, mode, ...) {
		#	as.mumeric and as.logical
		#	automatically apply decostand method!
		#	argument mode controls transpostion before
		#	caluclation of distances
		if (missing(mode)) mode = "Q"
		if (missing(binary)) {
			X <- as.numeric(m, mode = mode)
		} else {
			X <- as.logical(m, mode = mode)	
		}
		Xd <- vegan::vegdist(X, method = m@dist, ...)
		
		#	ensure dissimilarities
		if (max(Xd) > 1) Xd <- Xd / max(Xd)	
		
		#	assign attribute
		attributes(Xd) <- c(attributes(Xd), mode = toupper(mode))
		
		return(Xd)
	}
)

as.dist.Vegsoup <- function (m, ...) {
	vegsoup::as.dist(m, ...)
}
	
#	connectedness of dissimilarities
#	method for class VegsoupPartition, check inheritance should be absolete!
#	to do: documentation
setGeneric("getDistconnected",
	function (obj, ...)
		standardGeneric("getDistconnected")
)

setMethod("getDistconnected",
	signature(obj = "Vegsoup"),
	function (obj, ...) {
		distconnected(as.dist(obj), ...)
	}
)