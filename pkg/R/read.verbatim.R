#	function to import monospaced commuity tables
read.verbatim <- function (file, colnames, layers, replace = c("|", "-", "_"), species.only = FALSE, vertical = TRUE, verbose = FALSE) {

	if (missing(file))
		stop("plaese supply a path to a file")
		
	if (!missing(layers)) {
		if (!is.list(layers) & !is.character(layers) & !is.logical(layers)) {
			stop("layers must be a list or character vector", call. = FALSE)
		} else {
			if (is.list(layers)) {
				stopifnot(length(names(layers)) == length(layers))
				l <- rep(names(layers), lapply(layers, function (x) diff(x) + 1))
				stop("use @ assignment in inputfile", call. = FALSE)
			} else {
				if (is.character(layers)) {
					if (length(layers) == 1)
						at <- "@"
					else
						l <- layers
				}
			paste.layers <- TRUE # add layers to taxa
			with.layers <- TRUE  # prune layer from taxa
			}
		}
	} else {
		with.layers <- FALSE
	}

	#	read file
	txt <- readLines(file.path(file))
	
	#	test argument colnames
	test <- str_detect(txt, colnames)

	if (!any(test)) {
		stop("Did not find matches for argument colnames: ", colnames, ". Maybe misspelled?")
	}
	
	#	get position of keywords
	hb <- grep("BEGIN HEAD", txt)
	he <- grep("END HEAD", txt)
	tb <- grep("BEGIN TABLE", txt)
	te <- grep("END TABLE", txt)
	kw <- c(hb, he, tb, te)
	
	#	test for completeness
	test <- length(hb) != 1 | length(he) != 1 | length(tb) != 1 | length(te) != 1
	if (test) stop("did not find all keywords!", call. = FALSE)
		
	#	test tabs
	if (length(grep("\t", txt) > 0))
		stop("detected tab characters, please review your data.", call. = FALSE)
	
	#	replace
	if (length(replace) > 0) {
		for (i in 1:length(replace)) {
			txt <- sapply(txt,
				function (x) gsub(replace[ i ], " ", x, fixed = TRUE),
				USE.NAMES = FALSE)
		}	
	}
	
	#	find empty lines
	el <- sapply(txt, nchar, USE.NAMES = FALSE)
	
	#	find also uncomplete lines not representing data
	#	for example, lines consisting only of spaces
	ul <- el < median(el) & el != 0
	
	#	disregard keywords
	ul[kw] <- FALSE
	el <- el == 0
	el[ul] <- TRUE
	
	#	trim over long lines to the right
	txt <- gsub("\\s+$", "", txt)
	
	#	select elements with species abundances and header entries
	#	based on position of keywords and empty lines 
	ti <- hi <- rep(FALSE, length(txt))
	ti[ (tb + 1) : (te - 1) ] <- TRUE # species data
	hi[ (hb + 1) : (he - 1) ] <- TRUE # header data 
	ti[ el ] <- hi[ el ] <- FALSE	   # omit empty lines
		
	#	we now subset the sub-tables as a vector
	#	of strings for each line
	t.txt <- txt[ ti ] # species 
	h.txt <- txt[ hi ] # header
	
	#	test what we have done so far
	#	header may have trimming errors
	nc <- nchar(h.txt)
	test <- nc < median(nc)
	
	if (any(test)) h.txt[ test ] <- str_pad(h.txt[ test ], max(nc), side = "right")
	
	#	there might still remain inconsistencies in taxa block
	test0 <- nchar(t.txt)
	test1 <- table(test0)
	test2 <- which(names(test1)[min(test1)] == test0)
	error1 <- "length of characters is not the same on all lines!"
	error2 <- " check line(s) in taxa block: "
	
	if (length(test1) > 1) {
		stop(error1, error2,
			test2, str(t.txt[test2]),
			call. = FALSE)
	}
	
	#	test header
	test2 <- length(unique(nchar(h.txt))) > 1
	error <- "no success in pruning header section"
	if (test2) {
		stop(error, which(nchar(h.txt) != median(nchar(h.txt))), call. = FALSE)
	} #else {
		#if (!inherits(h.txt, "matrix")) {
		#	h.txt <- matrix(h.txt)
		#}
	#}
	
	#	the species	
	#	if passed all test proceed to cast to a mono spaced type matrix,
	#	where each cell is a single glyph, space or dot
	t.m <- matrix(" ", ncol = length(t.txt),
		nrow = max(sapply(t.txt, nchar)))
	vals <- sapply(t.txt, function (x) {
				sapply(1:nchar(x), function (y) substring(x, y, y), USE.NAMES = FALSE)
			}, simplify = FALSE, USE.NAMES = FALSE)
	t.m[] <- unlist(vals)
	t.m <- t(t.m)
	
	#	search for the beginning of the block of occurences values
	#	crude and prone to error!
	a <- apply(t.m, 2, function (x) length(grep("[[:alpha:]]", x))) 
	d <- apply(t.m, 2, function (x) length(grep("[[:punct:]]", x)))
	jj <- which(c(d - a) > 0)[1]
	
	#	layer assigments at the end of taxa string
	if (with.layers) {
		jat <- which(apply(t.m, 2, function (x) all(x == at)))
		if (length(jat) == 0) stop("could not resolve layer with symbol ", at, call. = FALSE)
		#	next blank
		t.at <- t.m[ , jat:ncol(t.m) ]
		s <- apply(t.at, 2, function (x) length(grep("[[:space:]]", x)))
		jj <- min(which(s > 0)) + jat
	}
	
	#	split species (txa) and data blocks	(val)
	txa <- str_trim(apply(t.m[, 1:(jj -1)], 1,
		function (x) paste(x, collapse = "")), side = "right")
	val <- t.m[ , jj: ncol(t.m) ]
	
	#	prune layer from taxa if present
	if (with.layers) {
		if (!paste.layers) {
			lay <- paste("  ", l, sep = "") # add two! leading spaces
			lay <- sapply(lay, function (x) grep(x, txa, fixed = TRUE))
			names(lay) <- layers
			if (length(unlist(lay)) != length(txa))
				stop("did not find all layer codes in all rows", call. = FALSE)
			for (i in layers)
				txa <- gsub(paste("  ", i, sep = ""), "", txa, fixed = TRUE)
			l <- rep(names(lay), sapply(lay, length))[ order(unlist(lay)) ]
			txa	<- str_trim(txa, side = "right")
		} else {
			if (length(layers) == 1) {
				l <- str_trim(sapply(strsplit(txa, at), "[[", 2), side = "right")
				txa	<- str_trim(sapply(strsplit(txa, at), "[[", 1), side = "right")
			}
		}
	}
	
	#	check for spaces as seperators
	n.space <- apply(val, 2,
		function (x) sum(sapply(x, function (y) y == " ")) )
		
	#	additional check for data integrity
	test <- which(n.space != nrow(t.m) & n.space != 0)
	
	if (verbose) cat("found ", nrow(t.m), " species")
		
	if (length(test) > 0) {
		message("some mono type character columns deviate from the expected pattern")
		message("please review your data and apply necessary changes")	
		for (i in test) {
			#	missing dot
			if (length(grep(".", val[,i], fixed = TRUE)) > 0) {
				error1 <- which(val[,i] == ".")
				error2 <- which(val[,i] == " ")
				stop("missing dot in species ",
					ifelse(length(error1) > length(error2), txa[error2], txa[error1]),
					" in column ", i + (jj - 1), call. = FALSE)	
			} else {
				stop("misplaced value in species",
					txa[which(val[,i] != " ")],
					"in column", i + (jj - 1), call. = FALSE)
			}		
		}
	} else {
		if (verbose) {
			cat("\nfound no obvious errors in species data block",
				"\nskip",
				 ifelse(is.na(table(n.space)[2]), 0, table(n.space)[2]),
				 "columns of blanks",
				 "and retain", dim(val[, n.space == 0])[2], "columns of data")
		}
	}
	
	#	strip of rows of blanks
	if (any(n.space == 0)) val <- val[, n.space == 0]
	
	if (verbose)
		cat("\ncharacters found in the species data block:",
			sort(unique(as.vector(val))), "\n")
	
	#	we are now ready to build the species matrix
	x <- data.frame(abbr = txa, val, stringsAsFactors = FALSE)
	
	#	the header
	
	#	test if any header data exept plot names are present
	test <- length(h.txt)
	if (test < 2) {
		if (verbose) cat("no header data found, just plot names")
		#	duplicate entry and name it dummy
		test <- str_detect(h.txt, colnames)
		if (test) {
			if (nchar("dummy") < nchar(colnames)) {
				d.txt <- gsub(colnames, str_pad("dummy", width = nchar(colnames), side = "right"), h.txt)
			} else {
				d.txt <- gsub(colnames, substring("dummy", 1, nchar(colnames)), h.txt)
			}
			
		} else {
			
		}
		
		h.txt <- c(h.txt, d.txt)
	}
	
	#	take care about vertical mode
	if (!vertical) { 
		#	we use object 'jj' here and hope that fits?
		par <- str_trim(substring(h.txt, 1, jj -1), "right")
		val <- substring(h.txt, jj, unique(nchar(h.txt)))
		val <- sapply(val, function (x) strsplit(x, "[[:blank:]]"))
		val <- t(sapply(val, function (x) x[x != ""]))
		dimnames(val) <- NULL

		#	the header
		if (nrow(val) > 1) {
			y <- data.frame(par, val, stringsAsFactors = FALSE)
		} else {
			y <- data.frame(par, X1 = as.vector(val), stringsAsFactors = FALSE)
		}	
		
		attr <- vector("list", length = length(par))
		names(attr) <- par
		
		if (ncol(x) != ncol(y)) stop("error parsing header with option vertical = ", vertical, call. = FALSE)
	}
	
	if (vertical) { # the default
		ne <- sapply(h.txt, nchar)
		#	groome strings if needed
		if (any(ne < median(ne))) {
			h.txt[ne < median(ne)] <-
				str_pad(h.txt[ne < median(ne)], width = median(ne), side = "right")
		}
		
		#	split variables (var) and data blocks (val)
		h.m <- matrix(" ", ncol = length(h.txt),
			nrow = max(sapply(h.txt, nchar)))
		vals <- sapply(h.txt, function (x) {
			sapply(1:nchar(x), function (y) substring(x, y, y), USE.NAMES = FALSE)
		}, simplify = FALSE, USE.NAMES = FALSE)
		h.m[] <- unlist(vals)
		h.m <- t(h.m)
		
		par <- str_trim(apply(h.m[, 1:(jj -1)], 1,
			function (x) paste(x, collapse = "")), side = "right")
		val <- h.m[, jj: ncol(h.m)]
		
		#	check for spaces as seperators
		#	hopefully less typos as in species matrix
		n.space <- apply(val, 2, function (x) sum(sapply(x, function (y) y == " ")) )
		
		#	and omit
		#	needs a second condition because there
		#	might be a blank in each column
		if (any(n.space == 0) | any(n.space == nrow(val)))
			val <- val[, n.space != nrow(val)]
		
		#	for sanity
		if (dim(val)[2] != (dim(x)[2] - 1)) {
			message("maybe set vertical = FALSE")
			stop("\nplease check your header data for misspelling or formatting", call. = FALSE)
		}
		
		#	header blocking variable
		for (i in 1:length(par)) {
			#	assume that the frist element is not empty
			#	last will be assigend at i == 1 and recycled
			if (par[i] == "") par[i] <- last else last <- par[i]
		}
		
		#	we are now ready to build the header matrix
		y <- data.frame(par, val, stringsAsFactors = FALSE)
		attr <- vector("list", length = length(unique(par)))
		names(attr) <- unique(par)
	}

	#	assign to attributes list,
	#	perform string grooming and type convert
	for (i in unique(par)) {
		test <- rle(par)$values
		if (any(duplicated(test))) {
			error <- test[duplicated(test)]
			stop("duplicates in header entries: ", error, call. = FALSE)
		}
			
		tmp <- str_trim(apply(y[y[, 1] == i, -1, drop = FALSE], 2, paste, collapse = ""))
		
		#	remove dots and drop leading zeros,
		#	but not for colnames (we preserve what is in the file)
		if (i == colnames)
			attr[[i]] <- tmp
		else
			attr[[i]] <- type.convert(gsub(".", "", tmp, fixed = TRUE), as.is = TRUE)	
	}
	
	#	finally assign abbr to rownames and turn into matrix
	#	test if rownames can be assigned
	if (length(unique(x[, 1])) != nrow(x) & !with.layers & !species.only) {
		warning("\nspecies are not unique.",
			" is the data structured in layers?",
			"\nreturn vector of species instead of matrix")
		x <- txa	
	} else {
		if (species.only) {
			x <- txa
		} else {
			#	paste layers
			if (with.layers) {
				rownames(x) <- paste(x[, 1], l, sep = "@")
			}
			else {
				rownames(x) <- x[, 1]
			}
			x <- x[, -1, drop = FALSE]
			x <- as.matrix(x)
	
			#	assign header as attribute
			#	assign plot ids
			if (!missing(colnames)) {
				cn <- attr[[colnames]]
				if (any(duplicated(cn))) {
					stop("duplicated colnames not allowed: ",
					paste(as.character(cn[duplicated(cn)]), collapse = " & "), call. = FALSE)
				}
				dimnames(x)[[2]] <- as.character(cn)
				attributes(x) <- c(attributes(x), attr)
			}
			else {
				dimnames(x)[[2]] <- NULL
				attributes(x) <- c(attributes(x), attr)
			}	
			class(x) <- c("VegsoupVerbatim", "matrix")
		}
	}
	
	return(x)
}

print.VegsoupVerbatim <- function (x) {
	print(as.data.frame(x))
}

#	function to append to class VegsoupVerbatim
read.verbatim.append <- function (x, file, mode = c("plots", "species", "layers"), collapse = ",", abundance) {

	message("read.verbatim.append is depreciated, use castFooter instead")
	if (!inherits(x, "VegsoupVerbatim")) {
		stop("plaese supply an object of class VegsoupVerbatim")
	}
	if (missing(file)) {
		stop("please supply a path to a file")
	}
	if (missing(mode)) {
		mode = "plots"
		message("missing mode, but set mode to ", mode)
	}
	else {
		MODES <- c("plots", "species", "layers")
		mode <- match.arg(mode, MODES)
		if (mode == "layers") {
			stop("mode \"layers\" not yet implemented!", call. = FALSE)
		}
	}
	if (!missing(abundance)) {
		stopifnot(length(abundance) == 1)
		if (mode == "species") {
			abundance <- as.character(abundance)
		}
		if (mode == "plots") {
			if (!is.logical(abundance)) {
				abundance <- as.logical(abundance)
			}
			abundance <- ifelse(is.na(abundance), TRUE, FALSE)
		}
	}
	else {
		if (mode == "species") {
			abundance = "+"
		}
		if (mode == "plots") {
			abundance = TRUE
		}
	}
	#	save attributes
	attr <- attributes(x)
	
	txt <- readLines(file.path(file))
	
	txt <- sapply(txt, function (x) {
			strsplit(x, ":", fixed = TRUE)
		}, USE.NAMES = FALSE)
	
	#	plain and simple uses dummy abundance
	if (mode == "species") {
		rn <- str_trim(unlist(lapply(txt, "[[", 1)))
		xx <- gsub("[[:blank:]]", "", unlist(lapply(txt, "[[", 2)))
		xx <- strsplit(xx, collapse, fixed = TRUE)
		names(xx) <- rn	
		y <- matrix(".", nrow = length(xx), ncol = ncol(x))
		colnames(y) <- colnames(x)
		rownames(y) <- rn
	
		for (i in 1:length(xx)) {
			#	i = 1
			ii <- match(names(xx)[i], rownames(y))
			stopifnot(length(ii) == 1)
			jj <- match(xx[[i]], colnames(y))
			y[ii, jj] <- abundance
		}
	}
	
	#	more structred, uses given abundance, but no layers
	if (mode == "plots") {
		cn <- str_trim(unlist(lapply(txt, "[[", 1)))
		xx <- str_trim(unlist(lapply(txt, "[[", 2)))
		xx <- sapply(strsplit(xx, collapse, fixed = TRUE), str_trim)
		tmp <- vector("list", length = length(xx))
		names(tmp) <- type.convert(cn) # handle leading zeros as in read.verbatim!
	
		for (i in 1:length(xx)) {
			ll <- sapply(xx[[i]], nchar)
			tmp[[i]] <- list(
				str_trim(substring(xx[[i]], 1, ll - 1)),
				substring(xx[[i]], ll, ll))
		}
		
		rn <- unique(unlist(lapply(tmp, "[[", 1)))
		y <- matrix(".", nrow = length(rn), ncol = ncol(x))
		colnames(y) <- colnames(x)
		rownames(y) <- rn
		for (i in 1:length(tmp)) {
			#	i = 1
			for (j in seq(along = tmp[[ i ]][[1]])) {
				#	j = 1
				ii <- match(tmp[[ i ]][[1]][ j ], rownames(y))
				jj <- match(names(tmp)[[ i ]], colnames(y))
				y[ii, jj] <- tmp[[ i ]][[2]][j]
			}
		}
	}

	test <- intersect(rownames(x), rownames(y))
	if (length(test) != 0) {
		stop("some species in file are already present in object x: ", test)
	}
	x <- rbind(x, y)

	class(x) <- c("VegsoupVerbatim", "matrix")
	attributes(x) <- c(attributes(x), attr[-c(1:2)])

	return(x)
}

#	accesor to get header data from VegsoupVerbatim objects
header <- function (x) {
	stopifnot(inherits(x, "VegsoupVerbatim"))
	r <- data.frame(attributes(x)[- c(1:2, length(attributes(x)))])
	rownames(r) <- colnames(x)
	return(r)
}
