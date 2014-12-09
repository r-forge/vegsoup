stackSpecies <- function (x, file, sep = ";", dec = ",", schema = c("abbr", "layer"), discard = c("taxon", "comment"), absences, zeros = FALSE, verbose = FALSE) {

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
	
	if (zeros) message("not implemented yet!")
	
	#	for safety
	x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
	n <- names(x)
	
	#	check schema
	test <- sapply(schema, function (y) any(y == n))
		
	if (!all(test)) {
		stop("can't stack object, did not find column(s): ",
			paste(schema[!test], collapse = " + "))
	}
	
	#	first guess of starting point of taxa block	
	j1 <- max(sapply(schema, function (y) which(y == n)))
			
	#	do we have other colums except schema, e.g. comment?	
	test <- sapply(discard, function (y) any(y == n))
	if (any(test)) {
		j0 <- unlist(sapply(discard, function (y) which(y == n)))
		if (any(max(j0) > j1)) j1 <- max(j0)
	}	
	
	#	subset only species abundances
	j <- c(c(j1 + 1):ncol(x))
	xx <- x[, j]
	
	#	check unique column labels
	if (!length(unique(names(xx)) == ncol(xx)))	{
		stop("plot columns are not unique")
	}
	
	plot <- rep(names(xx), each = nrow(xx))
	abbr <- rep(as.character(x[[schema[1]]]), ncol(xx))  # we've tested schema
	layer <- rep(as.character(x[[schema[2]]]), ncol(xx))
	cov <- as.vector(as.matrix(xx))
	
	#	test absences
	#	trust on matrix fill lower than 50%!
	if (missing(absences)) {
		absences <- table(cov)
		absences <- names(absences)[which.max(absences)]
	}
	
	test <- match(absences, unique(cov))
	if (any(is.na(test))) {
		stop("character \"", absences,
			"\" to code absences not found, but have: ", unique(cov))
	}
	else {
		if (verbose) cat("\n... absences are", absences)	
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
			cat("\n... groomed decimals, replaced colons with dots")
		}
	
	}
	
	#	check data type of abundances
	#	can become private function used in other places
	#	useless as long class "Species" only supports characters
	test <- type.convert(res$cov)
	
	if (class(test) == "factor" | class(test) == "character") {
		convert <- TRUE
		if (verbose) {
			cat("\n... cover seems to be ordinal: ")
			cat(names(table(test)), "\n")
		}
	}
	else {
		if (class(test) == "numeric" | class(test) == "integer") {
			if (class(test) == "integer" & dim(table(test)) == 2) {
				convert <- TRUE
				if (verbose) {
					cat("\n... cover seems to be logical (presence/absence)")
					cat(names(table(test)))
				}			
			}
			else {
				if (class(test) == "numeric" & dim(table(test)) > 2) {
					convert <- TRUE
					if (verbose) {
						cat("\n... cover seems to be continous: ")
						cat("\n    Tukey's five number summary:", fivenum(test), "\n")
					}
				}
				else {
					if (class(test) == "integer" & dim(table(test)) > 2) {
						convert <- TRUE
						if (verbose) {
							cat("\n... cover seems to be ordinal, coded with integers: ")
							cat(names(table(test)))
						}		
					}			
				}	
			}			
		convert <- TRUE
		}
	}
	
	if (convert)
		res$cov <- test
	else
		warning("unable to determine data type of species abundances", .call = FALSE)
	
	if (verbose) {
		cat("\n... data has", length(unique(res$layer)),
			"layer(s):", unique(res$layer))
	}
	
	#	leading spaces due to character conversion?
	res$cov <- gsub("[[:blank:]]", "", res$cov)
	
	#	leading zeros!
	#if (zeros) res[, 1] <- as.character(res[, 1]) else res[, 1] <- type.convert(res[, 1])
	#if (is.factor(res[,1])) res[, 1] <- as.character(res[, 1])
	
	res <- new("Species", data = res)	
	return(invisible(res))
}
