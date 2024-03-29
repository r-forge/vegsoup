if (!isGeneric("identical")) {
	setGeneric("identical", function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
	ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)
		standardGeneric("identical"))
}	


setMethod("identical", signature(x = "Species", y = "Sites"), 
function (x, y) {
	r <- sort(unique(x$plot)) == sort(unique(y$plot))
	if (all(r)) TRUE else FALSE
} )

setMethod("identical", signature(x = "Sites", y = "Species"), 
function (x, y) {
	identical(y, x)
} )

setMethod("identical", signature(x = "SpeciesTaxonomy", y = "Sites"), 
function (x, y) {
	x <- sort(unique(species(x)$plot)) # do we really need to call sort here?
	y <- sort(unique(y$plot))
	test <- x %in% y
	if (all(test))
		if (length(x) == length(y)) TRUE else FALSE
	else
		FALSE
} )

setMethod("identical", signature(x = "Species", y = "Taxonomy"), 
function (x, y) {
	x <- sort(unique(x$abbr)) # do we really need call sort here?
	y <- sort(unique(y$abbr))
	test <- x %in% y
	if (all(test))
		if (length(x) == length(y)) TRUE else FALSE
	else
		FALSE
} )

#setMethod("identical", signature(x = "Species", y = "Taxonomy"), 
#function (x, y) {
#	x <- sort(unique(x$abbr)) # do we really need call sort here?
#	y <- sort(unique(y$abbr))
#	test <- x %in% y
#	if (all(test))
#		if (length(x) == length(y)) TRUE else FALSE
#	else
#		FALSE
#} )
