#	cast species (and abundances) given in table footers
castFooter <- function (file, schema = c(":", "," , " "), first = TRUE, layers) {

	if (missing(file)) {
		stop("need a file name")
	}
	if (!missing(layers)) {
		at <- layers[1]
		layers <- TRUE
	}
	else {
		layers <- FALSE
	}
	
	#	argument to function
	#	order = c("plot", "species", "abundance")
	#ORDER <- c("plot", "species", "abundance")
	#order <- match.arg(order, ORDER, several.ok = TRUE)
	#if (length(order) != 3) stop("order must be of length 3")
	
	#order <- sapply(order, function (x) which(x == ORDER))
	
	#	seperate abundance value from species string
	.seperateFirst <- function (x, y) {
		m <- regexpr(y, x) # first schema match
		v <- str_trim(substring(x, 1, m))	# value
		s <- str_trim(substring(x, m + 1, nchar(x))) # species
		r <- cbind(v, s)
		colnames(r) <- NULL
		return(r)	
	}

	.seperateLast <- function (x, y) {
		r <- matrix("", nrow = length(x), ncol = 2)
		for (i in seq_along(x)) {
			p <- max(gregexpr(y, x[i])[[1]]) # position of last schema match
			v <- str_trim(substring(x[i], p + 1, nchar(x[i]))) # value
			s <- str_trim(substring(x[i], 1, p))	# species
			r[i, 1] <- v
			r[i, 2] <- s
		}
		return(r)	
	}
	
	x <- readLines(file)
	
	test <- which(x == "")
	if (length(test > 0)) {
		message("skip line(s) ", test)
		x <- x[-test]	
	}
	
	x <- strsplit(x, schema[1], fixed = TRUE)
	# plot
	p <- str_trim(sapply(x, "[[", 1))
	# species
	x <- str_trim(sapply(x, "[[", 2))
	x <- strsplit(x, schema[2], fixed = TRUE)
	x <- sapply(x, function (y) {
			sapply(y, function (z) {
				str_trim(z)
			}, USE.NAMES = FALSE)
		}, USE.NAMES = FALSE)
	# expand plot vector
	if (length(p) == 1)
		p <- rep(p, times = sum(sapply(x, length)))
	else
		p <- rep(p, times = sapply(x, length))
	
	#	cast string to values and species
	if (first)
		x <- sapply(x, function (xx) .seperateFirst(xx, schema[3]))
	else
		x <- sapply(x, function (xx) .seperateLast(xx, schema[3]))

	#	single relevee
	if (is.list(x))
		x <- do.call("rbind", x)
	else
		x <- t(x)

	r <- cbind(p, x)
	test <- nchar(r[,2])
	if (sum(test) != length(test)) {
		message("at least abundance values are not speperated properly")
	}
	colnames(r) <- c("plot", "cov", "taxon")

	if (layers) {
		x <- strsplit(r[,3], at)
		x2 <- 
		x2 <- 
		r <- cbind(r[, 1:2],
			taxon = sapply(x, "[", 1),
			layer = gsub(at, "", paste0(at, sapply(x, "[", 2))))
		r <- as.data.frame(r)[, c(1,3,4,2)]	# bring into order
	}
	else {
		r <- as.data.frame(r)
		r$layer = NA
		r <- as.data.frame(r)[, c(1,3,4,2)]	
	}
	return(r)
}
