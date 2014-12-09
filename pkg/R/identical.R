if (!isGeneric("identical")) {
	setGeneric("identical", function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE)
		standardGeneric("identical"))
}	


setMethod("identical", signature(x = "Species", y = "Sites"), 
function (x, y) {
	r <- sort(unique(x$plot)) == sort(unique(y$plot))
	if (all(r)) TRUE else sort(unique(x$plot))[r]
} )

setMethod("identical", signature(x = "Sites", y = "Species"), 
function (x, y) {
	intersect(y, x)
} )
