stackSpecies <- function (x, file, sep = ";", dec = ",", schema = c("abbr", "layer", "taxon"), absences, verbose = FALSE) {

	if (missing(x) & missing(file)) {
		stop("please supply either a data.frame or a csv file")	
	}
	
	if (!missing(file)) {
		if (is.character(file)) {
			x <- read.csv(file, sep = sep, dec = dec,
					colClasses = "character", check.names = FALSE)				
		}
	}
	else {
		if (is.data.frame(x) & missing(file)) {
	
			}
			else {
				stop("please supply a data.frame")	
		}
	}
	
	x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
	
	#	check schema
	abbr <- grep(schema[1], names(x)) #"abbr"
	layer <- grep(schema[2], names(x)) # "layer"
	taxon <- grep(schema[3], names(x)) # "taxon"
	
	#	test schema
	test <- length(abbr) > 0 & length(layer) > 0 & length(taxon) > 0
	
	if (!test) {
		if (length(abbr) < 1) {
			warning("did not find column", schema[1])		
		}
		if (length(layer) < 1) {
			warning("did not find column", schema[2])		
		}
		if (length(taxon) < 1) {
			warning("did not find column", schema[1])		
		}
		stop("can't stack object")
	}
	
	#	only species abundances
	sel <- c(max(c(abbr, layer, taxon)) + 1):ncol(x)
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
			taxon = "",
			stringsAsFactors = FALSE)
	
	if (length(grep(",", res$cov)) > 0) {
		res$cov <- gsub(",", ".", res$cov)
		if (verbose) {
			"\n... groomed decimals, replaced colons with dots"
		}
	
	}
	
	#	check data type of abundances
	#	can become private function used in other places
	#	useless as long class "Species" only supports characters
	test <- type.convert(res$cov)
	
	if (class(test) == "factor" | class(test) == "character") {
		convert <- TRUE
		cat("\n... cover seems to be ordinal: ")
		cat(names(table(test)), "\n")
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
					cat("\n    Tukey's five number summary:", fivenum(test), "\n")
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
	
	#	leading spaces due to character conversion?
	res$cov <- gsub("[[:blank:]]", "", res$cov)
	
	res <- new("Species", data = res)	
	return(invisible(res))
}
