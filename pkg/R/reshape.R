#	reshape tables where layers are in seperate columns
reshapeSpecies <- function (x, file, schema, sep = ";", dec = ",", verbose = FALSE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frmae or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		x <- read.csv(file, stringsAsFactors = FALSE,
			check.names = FALSE, sep = sep, dec = dec)
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

shapeSpecies <- function (obj) {
	if (!inherits(obj, "Vegsoup")) {
		stop("only defined for Vegsoup* objects")
	}
	spc <- species(species(obj)) #! get data slot
	#	data.frame to store results
	res <- as.data.frame(
    	     matrix("",
        	   ncol = length(Layers(obj)) + 2, # we need 2 more columns
	           nrow = sum(richness(obj, "sample"))),
    	   stringsAsFactors = FALSE)
	names(res) <- c("plot", "abbr", Layers(obj))

	res$plot <- rep(rownames(obj), richness(obj, "sample"))
	res$abbr <- unlist(sapply(rownames(obj), function (x) {
		Taxonomy(obj[rownames(obj) == x, ])$abbr}))
	#	slow
	for (i in 1:nrow(res)) {
		tmp <- res[i, ]
	    sel <- tmp$plot == spc$plot & tmp$abbr == spc$abbr
   		res[i, match(spc[sel, 3], names(res))] <- spc[sel, 4]
	}
	return(invisible(res))
}