#	retrieve distance matrix based on object settings (vegdist, decostand)
#	stats defines:
#   as.dist(m, diag = FALSE, upper = FALSE)
#	set old class
setOldClass("dist")

#if (!isGeneric("as.dist")) {
setGeneric("as.dist",
	function (m, diag = FALSE, upper = FALSE)
		standardGeneric("as.dist")
)
#}
setMethod("as.dist",
	signature(m = "Vegsoup"),
	function (m, diag, upper) { # dropped: mode
		#	as.mumeric and as.logical
		#	automatically apply decostand method!

		#	argument mode controls transposition before
		#	calculation of distances!
		#	unfortunately, this additional argument creates a new generic
		#	and proper dispatch is not guranted any more?
		#if (missing(mode)) {
			mode = "Q"
		#}
		Xd <- vegan::vegdist(as.matrix(m), method = vegdist(m), diag = diag, upper = upper) # ...
		
		#	ensure dissimilarities
		#if (vegdist(m) != "manhattan" & vegdist(m) != "euclidean") {
		#	if (max(Xd) > 1) Xd <- Xd / max(Xd)
		#}
		
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
	#as(m, "dist")
}


#if (!isGeneric("nndist")) {
setGeneric("nndist",
	function (X, ...)
		standardGeneric("nndist")
)
#}

setMethod("nndist",
	signature(X = "Vegsoup"),
	function (X, ...) {
		d <- as.matrix(as.dist(X))
		diag(d) <- 1
		nn <- apply(d, 1, which.min)
		diag(d) <- 0
		res <- apply(cbind(nn, 1:ncol(d)), 1,
			function (x) d[x[1], x[2]])
		attr(res, "neighbour") <- rownames(d)[nn]
		
		return(res)
	}
)   	