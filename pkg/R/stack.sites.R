#	stack sites data frame to match database structure
stackSites <- function (x, file, sep = ";", dec = ",", schema = "plot", zeros = FALSE, verbose = FALSE) {

	if (missing(x) & missing(file)) {
		stop("please supply either a data frame or a csv file")	
	}
	if (!missing(file)) {
		if (is.character(file)) {
			#	find schema column first
			x <- scan(file, what = "character", nlines = 1, sep = sep)
			j <- match(schema, x)
			
			cc <- rep(NA, length(j))
			cc[j] <- "character"
			if (is.na(j)) stop("schema not found, you might try another sep argument")
			
			#	we will loose leading zeros if we don't set colClasses = "character"	
			x <- read.csv(file, sep = sep, dec = dec,
				stringsAsFactors = FALSE, check.names = FALSE,
				colClasses = cc)
		}
	} else {
		if (is.data.frame(x) & missing(file)) {
				x <- x
			} else {
				stop("please supply a data.frame or use file argument")	
		}
	}
	if (length(schema) > 1) {
		schema <- schema[1]
		warning("use only first argument of schema", schema)	
	}	
	if (schema == "rownames") {
		x <- cbind(rownames = rownames(x), x)		
	} else {
		stopifnot(!is.na(match(schema, names(x))))
		if (!length(unique(x[[schema]])) == nrow(x))
			stop("schema column is not unique")		
	}
		
	#	all columns must be of mode character to use stack()
	res <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE,
		colClasses = "character")
	
	#	leading zeros!
	if (zeros) res[, 1] <- type.convert(res[, 1]) else res[, 1] <- as.character(res[,1])
	if (is.factor(res[, 1])) res[, 1] <- as.character(res[, 1])
		
	res.stack <- stack(res, stringsAsFactors = FALSE)
	
	plot <- res.stack[res.stack$ind == schema,]$values
	plot <- rep(plot, (nrow(res.stack)/length(plot)) - 1)
	res.stack <- res.stack[!res.stack$ind == schema,]
	res.stack <- data.frame(
		plot = as.character(plot),
		variable = as.character(res.stack[, 2]),
		value = as.character(res.stack[, 1]),
		stringsAsFactors = FALSE)
	res.stack <- res.stack[order(res.stack$plot),]
	res.stack[is.na(res.stack)] <- ""
	
	if (any(res.stack$plot == "")) {
		stop("please review your data")
	}
	rownames(res.stack) <- 1:nrow(res.stack)
	res <- res.stack
	
	if (verbose) {
		cat("found variables:", unique(res$variable), "\n")
	}	
	
	res <- new("Sites", data = res)	
	return(invisible(res))
}
