#as.dist(m, diag = FALSE, upper = FALSE)
#	set old class
setOldClass("dist")

#	need to drop mode argument!
#if (!isGnereic("as.dist")) {
setGeneric("as.dist",
	function (m, diag = FALSE, upper = FALSE) #, ... dropped
		standardGeneric("as.dist")
)
#}
setMethod("as.dist",
	signature(m = "Vegsoup"),
	function (m) { # dropped mode, ...
		#	as.mumeric and as.logical
		#	automatically apply decostand method!

		#	argument mode did formely control transposition before
		#	caluclation of distances
		#	unfortunately, this additional argument creates a new generic
		#	and proper dispatch is not guranted any more?
		#	e.g. affects typal()
		#	mode is silencecd for now
		#if (missing(mode)) mode = "Q"
		mode = "Q"
		Xd <- vegan::vegdist(as.matrix(m), method = m@dist) #, ...
		
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