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
		if (any(is.na(object@data[, 1:4]))) {
			FALSE
		} else {
			TRUE
		}				
	}
)

setMethod("initialize",
	"Species",
	function(.Object, data) {
		#	for safety and to get rid of factors
		#	improve speed, efficency
		data <- as.data.frame(
			as.matrix(data), stringsAsFactors = FALSE)				
		names(data)[1:4] <- c("plot", "abbr", "layer", "cov")
		#	valid strings
		data$abbr <- make.names(data$abbr)
		
		#	needs order, at least block of plots
		#data$plot <- as.character(data$plot)
				
		#	test for duplicated species
		#	robust test, disregard 'cov'
		input <- data[ ,c(1,2,3)]
		unique.input <- unique(input)            
		if (nrow(input) != nrow(unique.input)) {
			tmp <- data[duplicated(input), ][c("plot", "abbr")]
			tmp <- paste(paste(tmp[,1], tmp[,2]), collapse = "\n")

			warning("found duplicated species and dropped all duplicates",
				"\nplease review your data for observations:\n",
				tmp, call. = FALSE)
			
			data <- data[!duplicated(data[, c(1,2,3)]), ]
		}
		#	additional test
		if (dim(unique(unique(data)))[1] != dim(data)[1]) {
			warning("\n found duplicated species abundances for plots:\n... ",
				paste(data[duplicated(data), ]$plot, collapse = ", "),
				"\n apply unique(..., fromLast = FALSE) to get rid of duplicates!",
				"\n please review your data!", call. = FALSE)
			data <- unique(data, fromLast = FALSE)
		}
		#	test for missing cover
		if (any(data$cov == "") | any(is.na(data$cov)) ) {
			stop("missing cover for plots",
				"\nplease review your data!", call. = FALSE)
		}
		#	ensure valid names	
		#	data$abbr <- make.names(data$abbr)			
		rownames(data) <- 1:nrow(data)
		
	.Object@data <- data	
	return(invisible(.Object))	
	}
)