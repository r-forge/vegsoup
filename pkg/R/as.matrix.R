#	unfortunately as.dist dispatach for generic
#	with additional argument mode is not possible

#	vectorized internal function
.cast <- function (x, typeof) {
	ij <- indices(x, typeof)
	nc <- ncol(x)
	nr <- nrow(x)
	cv <- numeric(length = nc * nr)

	#	plots must be ordered for rle()!
	jj <- ij$j + rep(cumsum(rep(nc, nr)) - ncol(x), times = rle(ij$i)$length)
	cv[jj] <- ij$x
	
	return(matrix(cv, ncol = ncol(x), nrow = nrow(x),
		dimnames = ij$dimnames, byrow = TRUE))
}

#	return species matrix
setMethod("as.numeric",
	signature(x = "Vegsoup"),
	function (x, mode) {
		if (missing(mode)) mode <- "Q"
		MODE <- c("Q", "R")
		mode <- match.arg(toupper(mode), MODE)

		#	standardization as definded by decostand(x)
		stand <- slot(slot(x, "decostand"), "method")
		
		#	for efficency we get CAP before and then cast the matrix
		if (any(stand == "cap")) {
			#	we remove it from methods, because vegan::decostand can't handle it
			stand <- stand[stand != "cap"]
			if (length(stand) == 0) stand <- NULL
			#	no handle yet if second element is pa
			x <- cap(x, asVegsoup = TRUE)
		}

		#	cast matrix
		m <- .cast(x, "numeric")
		
		#	standardize if need
		if (!is.null(stand)) {
			if (length(stand) < 2) {
				if (stand == "wisconsin") {
					stand <- c("max", "total")
					m <- vegan::decostand(m, "max", 2)
					m <- vegan::decostand(m, "total", 1)
				}
				else {
					m <- vegan::decostand(m, stand)
				}
			}
			else {
				for (i in stand) {
						m <- vegan::decostand(m, i)
					}
			}
			attributes(m)$decostand <- stand
		}
	
	if (mode == "R") m <- t(m)
	
	return(invisible(m))
	}

)

setMethod("as.character",
	signature(x = "Vegsoup"),
	function (x, mode) {
		if (missing(mode)) mode <- "Q"
		MODE <- c("Q", "R")
		mode <- match.arg(toupper(mode), MODE)
		
		m <- .cast(x, "character")
		if (mode == "R") m <- t(m)
		return(invisible(m))
	}
)
	
setMethod("as.logical",
	signature(x = "Vegsoup"),
	function (x, mode) {
		if (missing(mode)) mode <- "Q"
		MODE <- c("Q", "R")
		mode <- match.arg(toupper(mode), MODE)
		#	we can conduct cap with presence/absence data

		#	standardization as definded by decostand(x)
		stand <- slot(slot(x, "decostand"), "method")
		
		#	for efficency we get CAP before casting the matrix
		if (any(stand == "cap")) {
			#	we remove it from methods, because vegan::decostand can't handle it
			stand <- stand[stand != "cap"]
			if (length(stand) == 0) stand <- NULL
			x <- cap(x, asVegsoup = TRUE)
		}
		
		m <- .cast(x, "logical")
		if (mode == "R") m <- t(m)
		storage.mode(m) <- "integer"
		return(invisible(m))
	}
)	

#if (!isGeneric("as.matrix")) {
#setGeneric("as.matrix",
#	function (x, ...)
#	standardGeneric("as.matrix"))
#}

setMethod("as.matrix",
	signature(x = "Vegsoup"),
	function (x, typeof, ...) { # ... argument mode
		if (missing(typeof)) typeof <- "numeric"
		TYPEOF <- c("character", "numeric", "logical")
		typeof <- match.arg(typeof, TYPEOF)

		if (typeof == "character") {
			m <- as.character(x, ...)
		}
		if (typeof == "numeric") {
			m <- as.numeric(x, ...)
		}
		if (typeof == "logical") {
			m <- as.logical(x, ...)
		}
		return(m)
	}
)

setAs(from = "Vegsoup", to = "matrix",
	def = function (from) {
		as.matrix(from)
		# typeof = "character", mode = "Q"
	}
)

#	ensure that also base functions dispatch properly
as.matrix.Vegsoup <-
	function (x, ...) as.matrix(x, ...) # as(x, "matrix")

if (!isGeneric("as.array")) {
setGeneric("as.array",
	function (x, ...)
	standardGeneric("as.array"))
}

#	return array of species matrix, one dimension for each layer
setMethod("as.array",
	signature(x = "Vegsoup"),
	function (x, typeof, ...) {	
	xx <- species(species(x)) #! get slot data
	scale <- coverscale(x) # rename local object scale to ?
	
	if (missing(typeof)) typeof <- "numeric"
	TYPEOF <- c("character", "numeric", "logical")
	typeof <- match.arg(typeof, TYPEOF)

	#	cover transformation
	if (typeof == "numeric" & !is.null(scale@codes)) {
		xx$cov <- as.numeric(as.character(
			factor(xx$cov, levels = scale@codes, labels = scale@lims)
			))
		if (any(is.na(xx$cov))) {
			stop("cover scale codes do not match data" )
		}
	}
	if (typeof == "numeric" & is.null(scale@codes)) {
		xx$cov <- as.numeric(xx$cov)
	}
	
	res <- table(xx[ c(1,2,3) ])

	#	insert values, not need for presence/absence ('logical')
	if (typeof == "numeric" | typeof == "character") {
		for (i in dimnames(res)$layer) {
			vals <- xx[xx$layer == i,]
			for (j in 1:nrow(vals)) {
				res[vals[j, 1], vals[j, 2], i] <- vals[j, 4]
			}
		}
	}
	
	#	ensure layer order
	#	order of species is alphabetic due to a call to table()
	return(res[, , layers(x)])
	}
)

setAs(from = "Vegsoup", to = "array",
	def = function (from) {
		as.array(from)
	}
)

#	ensure that also base functions dispatch properly
as.array.Vegsoup <-	function (x, ...) {
	as.array(x, ...)
}

#	return vector of abundances	
setMethod("as.vector",
	signature(x = "Vegsoup"), # , mode = "missing"
		function (x, mode) {
			if (missing(mode)) mode = "numeric"
				as.vector(as.matrix(x, typeof = mode))
})

#	ensure that base functions calling as.vector() work
as.vector.Vegsoup <- function (x, mode) {
	if (missing(mode)) mode = "numeric"
	as.vector(as.matrix(x, typeof = mode))
}	

#	locations and values of nonzero entries
#if (!isGeneric("indices")) {
setGeneric("indices",
	function (x, ...) # removed argument typeof from generic
	standardGeneric("indices"))	
#}
setMethod("indices",
	signature(x = "Vegsoup"),
		function (x, typeof) {
			if (missing(typeof)) {
				typeof <- "numeric"
			}
			TYPEOF <- c("character", "numeric", "logical")
			typeof <- match.arg(typeof, TYPEOF)
		
			cs <- coverscale(x)
			al <- sprintf("%s@%s", species(x)$abbr, species(x)$layer)
			ual <- colnames(x)
			pl <- species(x)$plot
			upl <- unique(pl)
			
			#	i,j vectors of the same length
			j <- match(al, ual)
			i <- as.integer(ordered(pl, levels = upl))
			
			if (typeof == "numeric" & !is.continuous(x)) {
				return(list(i = i, j = j,
					x = as.numeric(as.character(
						factor(species(x)$cov, cs@codes, cs@lims))),
					dimnames = list(upl, ual)))
			}
			if (typeof == "numeric" & is.continuous(x)) {
				return(list(i = i, j = j,
					x = as.numeric(species(x)$cov),
					dimnames = list(upl, ual)))
			}
			if (typeof == "character") {
				if (!is.continuous(x)) {
					#	message("coverscale has no codes")
				}
				return(list(i = i, j = j,
					x = species(x)$cov,	# is character by definition
					dimnames = list(upl, ual)))
			}
			if (typeof == "logical") {
				return(list(i = i, j = j,
					x = rep(1, nrow(species(species(x)))), #! use slot data
					dimnames = list(upl, ual)))
			}
		}
)

#	coerce to sparse matrix
#	very basic!
setAs(from = "Vegsoup", to = "sparseMatrix",
	def = function (from) {
		ij <- indices(from)
		res <- Matrix::sparseMatrix(i = ij$i, j = ij$j, x = as.integer(ij$x),
			dimnames = ij$dimnames)
		res
	}
)

#as.sparseMatrix.Vegsoup <- function (x, ...) {
#	as(x, "sparseMatrix")
#}
	
setAs(from = "Vegsoup", to = "dsparseMatrix",
	def = function (from) {
		ij <- indices(from)
		res <- Matrix::sparseMatrix(i = ij$i, j = ij$j, x = as.numeric(ij$x),
			dimnames = ij$dimnames)
		res
	}
)

setAs("Vegsoup", "list",
	def = function (from) {
		list(
		species = as.matrix(from, typeof = "character", mode = "Q"),
		sites = from@sites,
		taxonomy = from@taxonomy
		)
	}
)
