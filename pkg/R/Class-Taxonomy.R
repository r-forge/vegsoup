setClass("Taxonomy",
	representation(
	data = "data.frame")
)

setValidity("Taxonomy",
	method = function (object) {
		if (ncol(object@data) < 2) {
			FALSE
		} else {
			TRUE
		}
		if (identical(names(object@data)[1:2],
			c("abbr", "taxon"))) {
			TRUE	
		} else {
			FALSE
		}		
	}
)

setMethod("initialize",
	"Taxonomy",
	function(.Object, data) {
		#	depreciated
		#	for safety and to ensure validity		
		#data <- as.data.frame(
		#	as.matrix(data), stringsAsFactors = FALSE)[c("abbr", "taxon")]
		names(data)[1:2] <- c("abbr", "taxon")	
		#	alphabetic order
		data <- data[order(data$abbr), ]		
		#	ensure valid namesand promote rownames
		row.names(data) <- data$abbr <- make.names(data$abbr)

	.Object@data <- data	
	return(invisible(.Object))	
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
setMethod("taxonomy",
    signature(obj = "data.frame"),
    function (obj) {
    	new("Taxonomy", data = obj)
    }
    
)
setMethod("taxonomy",
    signature(obj = "matrix"),
    function (obj) {
    	new("Taxonomy",
    	data = as.data.frame(obj, stringsAsFactors = FALSE))
    }
    
)