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
		#	for safety and to ensure validity
		data <- as.data.frame(as.matrix(data), stringsAsFactors = FALSE)
		names(data)[1:3] <- c("plot", "variable", "value")
		#	only one plot variable possible
		data <- data[!duplicated(data[, 1:2]), ]
		###	order
		#data <- data[order(data$plot, data$variable), ]
		data$plot <- as.character(data$plot)

		.Object@data <- data	
	return(invisible(.Object))
	}
)
