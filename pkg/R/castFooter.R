#	cast species (and abundances) given in table footers
#	schema[ 1 ] prune either species or plot
#	schema[ 2 ] prune either abundance or plot

#	species.first = TRUE, abundance.first = FASLE, multiple = TRUE
#	Melica nutans: 262 +, 264 1
#	Carex aterima: 1, 109, 23

#	species.first = TRUE, abundance.first = TRUE, multiple = FALSE
#	Aster bellidiastrum: 1, 5

#	species.first = FALSE, abundance.first = FASLE
#	11: Aster bellidiastrum 1, Linum catharticum +, Lotus comiculatus +

#	species.first = FALSE, abundance.first = TRUE
#	16: + Aruncus dioicus, + Urtica dioica

#	species.first = FALSE, abundance.first = NA
#	1: Sisymbrium officinale, Sambucus nigra

castFooter <- function (file, schema = c(":", "," , " "), species.first = FALSE, abundance.first = TRUE, multiple = TRUE, abundance = "+", layers) {

	if (missing(file)) {
		stop("need a file name")
	}
	if (!missing(layers)) {
		at <- layers[1]
		layers <- TRUE
	} else {
		layers <- FALSE
	}

#	if (!is.logical(species.first)) {
#		stop("argument species.first must of mode logical")
#	}
	
#	if (is.na(abundance.first)) {
#		message("don't expect to find abundances, set to default: ", abundance)
#	} else {
#		if (!is.logical(abundance.first)) {
#			stop("argument abundance.first must be either of mode logical or NA")
#		}
#	}

#	if (!is.logical(multiple)) {
#		stop("argument species.first must of mode logical")
#	}

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
	
	.seperate <- function (x, y) {
		r <- strsplit(x, y)
		r <- sapply(r, str_trim, simplify = FALSE)
		return(r)
	}
	
	x <- readLines(file)
	
	test <- which(x == "")
	if (length(test > 0)) {
		message("skip line(s) ", test)
		x <- x[ -test ]	
	}
	
	# split schema[1]

	
	if (species.first) { # genu spec: 10 +, 32 1 \n or genu spec: 10, + \n
		
		xx <- strsplit(x, schema[ 1 ], fixed = TRUE)
		
		if (multiple) { # genu spec: 10 +, 32 1 \n
			
			s <- str_trim(sapply(xx, "[[", 1)) # species
			pa <- str_trim(sapply(xx, "[[", 2)) # plots (and abundances) or vice-versa 
			pa <- strsplit(pa, schema[ 2 ], fixed = TRUE)
			pa <- sapply(pa, str_trim, simplify = FALSE)
			s <- rep(s, times = sapply(pa, length))
		
			if (is.na(abundance.first)) {
				p <- unlist(pa)
				a <- rep(abundance, length(s))
			} else {
				if (abundance.first) {
					sa <- sapply(pa, function (x) .seperateFirst(x, schema[ 3 ]))
					p <- unlist(sapply(sa, function (x) x[ ,2] ))
					a <- unlist(sapply(sa, function (x) x[ ,1] ))		
				} else {
					sa <- sapply(pa, function (x) .seperateLast(x, schema[ 3 ]))
					p <- unlist(sapply(sa, function (x) x[ ,2] ))
					a <- unlist(sapply(sa, function (x) x[ ,1] ))		
				}
			}
		}
		
		if (!multiple) { # genu spec: 10, + \n
			s <- str_trim(sapply(xx, "[[", 1)) # species
			pa <- str_trim(sapply(xx, "[[", 2)) # plots (and abundances) or vice-versa
			
			if (abundance.first) {
				pa <- sapply(pa, function (x) .seperate(x, schema[ 3 ]))
				p <- sapply(pa, "tail", 1)
				a <- sapply(pa, "head", 1)
			} else {
				pa <- sapply(pa, function (x) .seperate(x, schema[ 3 ]))
				p <- sapply(pa, "head", 1)
				a <- sapply(pa, "tail", 1)
			}
						
		}
	}
	
	if (!species.first) { # 10: genu spec, genu spec \n

		xx <- strsplit(x, schema[ 1 ], fixed = TRUE)
		
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
				s <- unlist(sapply(sa, function (x) x[ ,2] ))
				a <- unlist(sapply(sa, function (x) x[ ,1] ))		
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
