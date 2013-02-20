
#	reshape tables where layers are in seperate columns
reshape.species <- function (x, file, schema, csv2 = TRUE, verbose = TRUE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frmae or a csv file")	
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
			#	for safety
			x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
		} else {
			stop("please supply a data.frame or use file argument")	
	}
}

if (!missing(schema)) {
	layers <- schema[3:length(schema)]
	plot.abbr <- match(schema[1:2], names(x))
	layers <- names(x)[-plot.abbr]
	if (verbose) {
		cat("attempt to use columns:", layers, "as layer")	
	}
} else {
	stop("please supply schema")
}

res <- x

layers <- data.frame(layers, index = match(layers, names(x)),
	stringsAsFactors = FALSE)

res <- c()
for (i in 1:nrow(layers)) {
	res <- rbind(res,
	cbind(layers[i, 1], as.matrix(x[, c(plot.abbr, as.integer(layers[i, 2]))])))
}

res <- as.data.frame(res,
	stringsAsFactors = FALSE)
res <- res[,c(2,3,1,4)]
names(res) <- c("plot", "abbr", "layer", "cov")

res <- res[res$cov != "0",]
res <- res[res$cov != "",]

res <- res[order(res$plot, res$abbr, res$layer),]

res <- new("Species", data = res)
return(invisible(res))
}