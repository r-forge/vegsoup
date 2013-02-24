#	convert between matrix formats for import
#	rename to Species
stack.species <- function (x, file, csv2 = TRUE, schema = c("abbr", "layer", "comment"), absences, verbose = FALSE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frame or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				colClasses = "character", check.names = FALSE)				
		} else {
			x <- read.csv(file,
				colClasses = "character", check.names = FALSE)				
		}
	}
} else {
	if (is.data.frame(x) & missing(file)) {

		} else {
			stop("please supply a data.frame")	
	}
}

x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)

#	check schema
abbr <- grep(schema[1], names(x)) #"abbr"
layer <- grep(schema[2], names(x)) # "layer"
comment <- grep(schema[3], names(x)) # "comment"

#	test schema
test <- length(abbr) > 0 & length(layer) > 0 & length(comment) > 0

if (!test) {
	if (length(abbr) < 1) {
		warning("did not find column abbr")		
	}
	if (length(layer) < 1) {
		warning("did not find column abbr")		
	}
	if (length(comment) < 1) {
		warning("did not find column comment")		
	}
	stop("can't coerce object")
}

abbr <- grep("abbr", names(x))
layer <- grep("layer", names(x))
comment <- grep("comment", names(x))

#	only species abundances
sel <- c(max(c(abbr, layer, comment)) + 1):ncol(x)
xx <- x[, sel]

#	check unique column labels
if (!length(unique(names(xx)) == ncol(xx)))	{
	stop("plot columns are not unique")
}

plot <- rep(names(xx), each = nrow(xx))
abbr <- rep(as.character(x$abbr), ncol(xx))
layer <- rep(as.character(x$layer), ncol(xx))
cov <- as.vector(as.matrix(xx))

#	test absences
#	trust on matrix fill lower than 50%!
if (missing(absences)) {
	absences <- table(cov)
	absences <- names(absences)[which.max(absences)]
}

test <- match(absences, unique(cov))
if (any(is.na(test))) {
	stop("character \"", absences, "\" to code absences not found, but have: ", unique(cov))
} else {
	cat("\n... absences are", absences)	
	ij <- cov != absences
}

res <- data.frame(
		plot = as.character(plot)[ij],
		abbr = as.character(abbr)[ij],
		layer = as.character(layer)[ij],
		cov = as.character(cov)[ij],
		comment = "",
		stringsAsFactors = FALSE)

if (length(grep(",", res$cov)) > 0) {
	res$cov <- gsub(",", ".", res$cov)
	if (verbose) {
		"\n... groomed decimals, replaced colons with dots"
	}

}

#	check data type of abundances
#	can become private function used in other places
#	useless as long obj@species only supports characters
test <- type.convert(res$cov)

if (class(test) == "factor" | class(test) == "character") {
	convert <- TRUE
	cat("\n... cover seems to be ordinal: ")
	cat(names(table(test)))
} else {
	if (class(test) == "numeric" | class(test) == "integer") {
		if (class(test) == "integer" & dim(table(test)) == 2) {
			cat("\n... cover seems to be logical (presence/absence)")
			cat(names(table(test)))
			convert <- TRUE			
		} else {
			if (class(test) == "numeric" & dim(table(test)) > 2) {
				convert <- TRUE
				cat("\n... cover seems to be continous: ")
				cat("\n    Tukey's five number summary:", fivenum(test))
			} else {
				if (class(test) == "integer" & dim(table(test)) > 2) {
					convert <- TRUE
					cat("\n... cover seems to be ordinal, coded with integers: ")
					cat(names(table(test)))		
				}			
			}	
		}			
	}
}

if (convert) {
	res$cov <- test
} else {
	warning("unable to determine data type of species abundances", .call = FALSE)
}	

if (verbose) {
	cat("\n... data has", length(unique(res$layer)),
		"layer(s):", unique(res$layer))
	}
	
res <- new("Species", data = res)	
return(invisible(res))
}
