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
    	if (is.na(method)) 
        	stop("invalid distance method")
    	if (method == -1) 
        	stop("ambiguous distance method")
		x@dist <- method
		x	
	}
)	

#	set old class
setOldClass("dist")
#	retrieve distance matrix
setGeneric("as.dist",
	function (m, diag = FALSE, upper = FALSE, ...)
		standardGeneric("as.dist")
)
setMethod("as.dist",
	signature(m = "Vegsoup"),
	function (m, mode, ...) {
		#	as.mumeric and as.logical
		#	automatically apply decostand method!
		#	argument mode controls transposition before
		#	caluclation of distances
		if (missing(mode)) mode = "Q"
		Xd <- vegan::vegdist(as.matrix(m), method = m@dist, ...)
		
		#	ensure dissimilarities
		if (max(Xd) > 1) Xd <- Xd / max(Xd)	
		
		#	assign attribute
		attributes(Xd) <- c(attributes(Xd), mode = toupper(mode))
		
		return(Xd)
	}
)
setAs(from = "Vegsoup", to = "dist",
	def = function (from) {
		vegsoup::as.dist(from)
	}
)
as.dist.Vegsoup <- function (m, ...) {
	vegsoup::as.dist(m, ...)
}
	
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