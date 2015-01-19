if (!isGeneric("identical")) {
	setGeneric("identical", function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE)
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
	r <- sort(unique(species(x)$plot)) %in% sort(unique(y$plot))
	if (all(r)) TRUE else FALSE
} )

setMethod("identical", signature(x = "Species", y = "Taxonomy"), 
function (x, y) {
	r <- sort(unique(x$abbr)) %in% sort(unique(y$abbr))
	if (all(r)) TRUE else FALSE
} )
