setClass("Species",
	representation(
	data = "data.frame")
)

setValidity("Species",
	method = function (object) {
		if (identical(names(object@data)[1:4],
			c("plot", "abbr", "layer", "cov"))) {
			TRUE	
		} else {
			FALSE
		}		
	}
)

#	get slot species
setGeneric("species",
	function (obj, ...)
		standardGeneric("species")
)
setMethod("species",
    signature(obj = "Species"),
    function (obj) obj@data
)

#setMethod("initialize",
#         "Species",
#          function(.Object, data = data.frame()) {
#              .Object@data <- data
#           .Object
#         }
#)
