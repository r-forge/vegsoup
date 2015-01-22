#	plot method
#if (!isGeneric("plot")) {
setGeneric("plot", function(x, y, ...)
	standardGeneric("plot"))
#}	
#if (!isGeneric("plot")) {
setGeneric("plot", function(x, y, ...)
	standardGeneric("plot"))
#}	
  
setMethod("plot",
    signature(x = "Vegsoup", y = "missing"),
	function (x, ...) {
	opar <- par(mfrow = c(2,2))

	r1 <- richness(x, "sample")
	r2 <- colSums(x)
	r3 <- table(species(x)$cov)
	r4 <- table(species(x)$layer)
	
	#	resort to coverscale
	if (is.ordinal(x)) {
		j <- match(coverscale(x)@codes, names(r3))
		j <- j[!is.na(j)] # we might miss a layer
		r3 <- as.table(r3[j])
	}
		
		
	hist(r1, main = "Species richness", xlab = "Number of species")        
	hist(r2, main = "Species occurences", xlab = "Number of occurences")        
	plot(r3 / sum(r3), main = "Coverscale", ylab = "Fraction")
	plot(r4 / sum(r4), main = "Layers", ylab = "Fraction")

	on.exit(par(opar))
} )

#	plotting methods for hist()
setMethod("hist",
	signature(x = "Vegsoup"),
	function (x, ...) {
		fig <- hist(colSums(x), ...)
		return(invisible(fig))
	}
)

#	plotting method hist
setMethod("hist",
	signature(x = "VegsoupPartition"),
	function (x, ...) {
		fig <- hist(richness(x, "partition"), ...)
		return(invisible(fig))
	}

)

setMethod("hist",
	signature(x = "VegsoupPartitionFidelity"),
	function (x, ...) {
		fig <- hist(x@stat, ...)
		return(invisible(fig))
	}

)