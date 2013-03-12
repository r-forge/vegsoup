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
		data <- as.data.frame(
			as.matrix(data), stringsAsFactors = FALSE)

		#	bring columns into order
		
		if (dim(data)[2] > 2) {
 			sel.abbr.taxon <- match(c("abbr", "taxon"), names(data))
			sel <- 1:length(names(data))
			data <- data[c(sel[sel.abbr.taxon], sel[-sel.abbr.taxon])]
		} else {
			names(data)[1:2] <- c("abbr", "taxon")
		}
		stopifnot(length(unique(data$abbr)) == nrow(data))
		#	valid strings
		data$abbr <- make.names(data$abbr)
		#	alphabetic order
		data <- data[order(data$abbr), ]		
		#	ensure valid namesand promote rownames
		row.names(data) <- data$abbr <- make.names(data$abbr)

	.Object@data <- data	
	return(invisible(.Object))	
	}
)