setClass("Species",
	representation(
	data = "data.frame")
)

setValidity("Species",
	method = function (object) {
		if (ncol(object@data) < 4) {
			FALSE
		} else {
			TRUE
		}
		if (identical(names(object@data)[1:4],
			c("plot", "abbr", "layer", "cov"))) {
			TRUE	
		} else {
			FALSE
		}				
	}
)

setMethod("initialize",
	"Species",
	function(.Object, data) {
		#	depreciated
		#	for safety and to ensure validity
		#data <- as.data.frame(
		#	as.matrix(data), stringsAsFactors = FALSE)[c("plot", "abbr", "layer", "cov")]
					
		names(data)[1:4] <- c("plot", "abbr", "layer", "cov")
		
		#	order
		data <- data[order(data$plot, data$layer, data$abbr), ]
		
		if (any(regexpr("[[:alpha:]]", data$plot) < 1)) {
				warning("\n ... plot identifier contains only numbers, ",
					"\nbut will be coerced to character!", call. = FALSE)	
			data$plot <- as.character(data$plot)
		}			
		#	test for duplicated species
		#	robust test            
		if (nrow(data[,c(1,2,3)]) != nrow(unique(data[,c(1,2,3)]))) {
			warning("\n found duplicated species for plots: ",
				"\n... ", paste(data[duplicated(data[, c(1,2,3)]), ]$plot, collapse = " "),
				"\n... ", paste(data[duplicated(data[, c(1,2,3)]), ]$abbr, collapse = " "),
				"\n drop all duplicates in x!",
				"\n they will confuse me otherwise?",
				"\n please review your data!", call. = FALSE)
			data <- data[!duplicated(data[, c(1,2,3)]), ]
		}
		#	additional test
		if (dim(unique(unique(data)))[1] != dim(data)[1]) {
			warning("\n found duplicated species abundances for plots:\n... ",
				paste(data[duplicated(data), ]$plot, collapse = ", "),
				"\n apply unique(..., fromLast = FALSE) to get rid of duplicates!",
				"\n they will confuse me otherwise?",
				"\n please review your data!", call. = FALSE)
			data <- unique(data, fromLast = FALSE)
		}
		#	ensure valid names	
		data$abbr <- make.names(data$abbr)			
		rownames(data) <- 1:nrow(data)
		
	.Object@data <- data	
	return(invisible(.Object))	
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

#setReplaceMethod("Layers",
#	signature(obj = "Species", value = "ANY"),
#	function (obj, value) {
#		if (length(value) != length(Layers(obj))) {
#			stop("length of value does not match length layers of object")
#		}
#		if (any(!Layers(obj) %in% value)) {
#			stop("items of value do not match layers of object",
#				"\n use Layers(obj, collapse = value),",
#				" where layers to be dropped are coded as NA") 
#		}
#		obj@layers <- value
#		return(obj)		
#	}
#)
