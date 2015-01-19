if (!isGeneric("intersect")) {
	setGeneric("intersect", function (x, y)
		standardGeneric("intersect"))
}	


setMethod("intersect", signature(x = "Species", y = "Sites"), 
function (x, y) {
	intersect(sort(unique(x$plot)), sort(unique(y$plot)))
} )

setMethod("intersect", signature(x = "Sites", y = "Species"), 
function (x, y) {
	intersect(y, x)
} )

setMethod("intersect", signature(x = "SpeciesTaxonomy", y = "Sites"), 
function (x, y) {
	intersect(sort(unique(species(x)$plot)), sort(unique(y$plot)))
} )

setMethod("intersect", signature(x = "Species", y = "Taxonomy"), 
function (x, y) {
	intersect(sort(unique(species(x)$abbr)), sort(unique(z$abbr)))
} )


