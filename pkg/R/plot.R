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
	on.exit(par(opar))
	
	plt <- Species(x)$plot		
	richness <- aggregate(rep(1, length(plt)),
		by = list(plt), sum)$x
	hist(richness, xlab = "Species richness", ...)
    
	spc <- Species(x)$abbr
	occurences <- aggregate(rep(1, length(spc)),
		by = list(spc), sum)$x
    
	hist(occurences, xlab = "Species occurences", ...)
	res <- list(richness, occurences)
	return(invisible(res))
}
)
