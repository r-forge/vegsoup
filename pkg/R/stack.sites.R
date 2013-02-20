#	stack sites data frame to match database structure
#	rename to Sites
stack.sites <- function (x, file, csv2 = TRUE, schema = "plot", verbose = FALSE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frame or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)

		} else {
			x <- read.csv(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		}
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

stopifnot(!is.na(match(schema, names(x))))	

#	all columns must be of mode character to  use stack()
res <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE,
	colClasses = "character")
	
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
	cat("found variables:", unique(res$variable))
}	

res <- new("Sites", data = res)	
return(invisible(res))
}
