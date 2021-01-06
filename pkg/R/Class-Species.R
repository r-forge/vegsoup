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
		#	as.matrix has effect if abbr is numeric
		data <- as.data.frame(as.matrix(data), stringsAsFactors = FALSE)
		names(data)[1:4] <- c("plot", "abbr", "layer", "cov")
		#	valid strings
		data$abbr <- make.names(data$abbr)
		data$plot <- gsub("[[:blank:]]", "", data$plot)
		#	test for duplicated species
		#	robust test, disregard 'cov'
		input <- data[ ,c(1,2,3)]
		unique.input <- unique(input)			
		if (nrow(input) != nrow(unique.input)) {
			test <- data[duplicated(input), ][c("plot", "abbr")]
			test <- paste(paste(test[,1], test[,2]), collapse = "\n")

			message("found duplicated species and dropped all duplicates",
				"\nplease review your data for observations:\n", test)
			
			data <- data[!duplicated(data[, c(1,2,3)]), ]
		}
		#	additional test
		if (dim(unique(unique(data)))[1] != dim(data)[1]) {
			message("\n found duplicated species abundances for plots:\n... ",
				paste(data[duplicated(data), ]$plot, collapse = ", "),
				"\n apply unique(..., fromLast = FALSE) to get rid of duplicates!",
				"\n please review your data!")
			data <- unique(data, fromLast = FALSE)
		}
		#	test for missing cover
		test1 <- data$cov == ""
		test2 <- is.na(data$cov)
		if (any(test1) | any(test2)) {
			message("missing cover for plots")
			if (nrow(data[test1, ]) > 0) {
				print(data[test1, ])	
			}
			if (nrow(data[test2, ]) > 0) {
				print(data[test2, ])
			}	
			stop("please review your data!", call. = FALSE)
		}
		#	ensure valid data frame row names					
		rownames(data) <- seq_len(nrow(data))
		
	.Object@data <- data	
	return(invisible(.Object))	
	}
)