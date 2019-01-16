#	cast species (and abundances) given in table footers
castFooter <- function (file, schema = c(":", "," , " "), species.first = FALSE, abundance.first = TRUE, abundance = "+", layers) {

	if (missing(file)) {
		stop("need a file name")
	}
	if (!missing(layers)) {
		at <- layers[1]
		layers <- TRUE
	} else {
		layers <- FALSE
	}
	
	#	seperate abundance value from species string
	.seperateFirst <- function (x, y) {
		m <- regexpr(y, x) # first schema match
		v <- str_trim(substring(x, 1, m)) # value
		s <- str_trim(substring(x, m + 1, nchar(x))) # species
		r <- cbind(v, s)
		colnames(r) <- NULL
		return(r)	
	}

	.seperateLast <- function (x, y) {
		r <- matrix("", nrow = length(x), ncol = 2)
		for (i in seq_along(x)) {
			p <- max(gregexpr(y, x[ i ])[[ 1 ]]) # position of last schema match
			v <- str_trim(substring(x[ i], p + 1, nchar(x[ i ]))) # value
			s <- str_trim(substring(x[ i ], 1, p)) # species
			r[ i, 1 ] <- v
			r[ i, 2 ] <- s
		}
		return(r)	
	}
	
	x <- readLines(file)
	
	test <- which(x == "")
	if (length(test > 0)) {
		message("skip line(s) ", test)
		x <- x[ -test ]	
	}
	
	# split schema[1]
	xx <- strsplit(x, schema[ 1 ], fixed = TRUE)
	
	if (species.first) { # genu spec: 10, 32
		s <- str_trim(sapply(xx, "[[", 1)) # species
		pa <- str_trim(sapply(xx, "[[", 2)) # plots (and abundances)
		pa <- sapply(strsplit(pa, schema[ 2 ], fixed = TRUE), str_trim)
		s <- rep(s, times = sapply(pa, length))
		
		if (is.na(abundance.first)) {
			p <- unlist(pa)
			a <- rep(abundance, length(s))
		} else {
			stop("not implemented yet")
		}
	} else { # 10: genu spec, genu spec
		# plot
		p <- str_trim(sapply(xx, "[[", 1))	
		# species (and abundance)
		sa <- str_trim(sapply(xx, "[[", 2))
		sa <- strsplit(sa, schema[ 2 ], fixed = TRUE)
		sa <- sapply(sa, function (y) {
				sapply(y, function (z) {
					str_trim(z)
				}, USE.NAMES = FALSE)
			}, USE.NAMES = FALSE)
		# expand plot vector
		if (length(p) == 1) {
			p <- rep(p, times = sum(sapply(sa, length)))
		} else {
			p <- rep(p, times = sapply(sa, length))
		}
		#	cast string to values and species
		if (is.na(abundance.first)) {
			s <- unlist(sa)
			a <- rep(abundance, length(s))
		} else {
			if (abundance.first) {	
				sa <- sapply(sa, function (x) .seperateFirst(x, schema[ 3 ]))
			} else {
				sa <- sapply(sa, function (x) .seperateLast(x, schema[ 3 ]))
			}
		}		
	}
	
	#	single relevee
	if (is.list(x))	x <- do.call("rbind", x) else x <- t(x)

	if (layers) {
		stop("new implementation")
		x <- strsplit(s, at)
		s <- sapply(sapply(x, "[", 1), str_trim)
		l <- sapply(sapply(x, "[", 2), str_trim)
	} else {
		l <- "0l"
	}

	r <- species(cbind(p, s, l, a))

	return(r)
}
