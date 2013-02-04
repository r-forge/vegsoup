setClass("Taxonomy",
	representation(
	data = "data.frame")
)

setValidity("Taxonomy",
	method = function (object) {
		if (identical(names(object@data)[1:2],
			c("abbr", "taxon"))) {
			TRUE	
		} else {
			FALSE
		}
		
#		if (dim(unique(object)) == dim(object)) {
#			TRUE
#		} else {
#			FALSE
#		}		
	}
)

#	accessor method
setGeneric("taxonomy",
	function (obj, ...)
		standardGeneric("taxonomy")
)
setMethod("taxonomy",
    signature(obj = "Taxonomy"),
    function (obj) obj@data
)

#setMethod("initialize",
#         "Taxonomy",
#          function(.Object, data = data.frame()) {
#              .Object@data <- data
#           .Object
#         }
#)