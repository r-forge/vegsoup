setClass("Sites",
	representation(
	data = "data.frame")
)

setMethod("initialize",
	"Sites",
	function(.Object, data) {
		#	for safety and to ensure validity
		data <- as.data.frame(as.matrix(data), stringsAsFactors = FALSE)
		names(data)[1:3] <- c("plot", "variable", "value")
		#	only one plot variable possible
		data <- data[!duplicated(data[, 1:2]), ]
		#	ensure charcters
		data$plot <- as.character(data$plot)
		#	order
		data <- data[order(data$plot, data$variable), ]

		.Object@data <- data	
	return(invisible(.Object))
	}
)

#	currently not used
setValidity("Sites",
	method = function (object) {
		schema <- c("plot", "variable", "value")
		if (ncol(object@data) < 3) FALSE else TRUE		
		if (identical(names(object@data)[1:3], schema)) TRUE else FALSE
	}
)
