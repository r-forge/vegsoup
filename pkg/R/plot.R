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

	opar <- par(mfrow = c(1,2))
	
	hist(richness(x, "sample"), ...)
        
	hist(colSums(x), ...)
	
	par(opar)
}
)

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
		fig <- hist(richness(prt, "partition"), ...)
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