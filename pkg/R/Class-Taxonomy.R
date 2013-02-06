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
setMethod("show",
    signature(object = "Taxonomy"),
    function (object) {
		cat("object of class", class(object))
		cat("\nnumber of taxa", nrow(object@data))
		cat("\nshow only first",
			ifelse(nrow(object@data) <= 6, nrow(object@data), 6),
			"rows\n\n")
		print(head(object@data, n = 6L))
    }
)
setMethod("[",
    signature(x = "Taxonomy",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
    	taxonomy(x@data[i, j, ...])
    }
)
setMethod("$", "Taxonomy", 
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)    	
