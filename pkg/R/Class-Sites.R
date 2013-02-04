setClass("Sites",
	representation(
	data = "data.frame")
)

setValidity("Sites",
	method = function (object) {
		if (ncol(object@data) < 3) {
			FALSE
		} else {
			TRUE
		}		
		if (identical(names(object@data)[1:3],
			c("plot", "variable", "value"))) {
			TRUE	
		} else {
			FALSE
		}		
	}
)

setMethod("initialize",
	"Sites",
	function(.Object, data) {
		#	depreciated
		#	for safety and to ensure validity
		#	data <- as.data.frame(
		#	as.matrix(data), stringsAsFactors = FALSE)[c("plot", "variable", "value")]
		
		names(data)[1:3] <- c("plot", "variable", "value")
		data <- data[order(data$plot, data$variable), ]
		
		if (any(regexpr("[[:alpha:]]", data$plot) < 1)) {
				warning("\n ... plot identifier contains only numbers, ", 
					"\nbut will be coerced to character!", call. = FALSE)	
			data$plot <- as.character(data$plot)
		}
		.Object@data <- data	
	return(invisible(.Object))
	}
)
		
#	get slot
setGeneric("sites",
	function (obj, ...)
		standardGeneric("sites")
)
setMethod("sites",
    signature(obj = "Sites"),
    function (obj) obj@data
)