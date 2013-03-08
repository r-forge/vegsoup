
#	return species matrix
setMethod("as.numeric",
    signature(x = "Vegsoup"),
    function (x, mode) {
    	if (missing(mode)) mode <- "Q"
    	MODE <- c("Q", "R")
    	mode <- match.arg(toupper(mode), MODE)
    	m <- .cast(x, mode = 1)
		#	standardization as definded by decostand(x)		
		stand <- slot(slot(x, "decostand"), "method")
    	
		if (!is.null(stand)) {
			if (length(stand) < 2) {
				if (stand == "wisconsin") {
					stand <- c("max", "total")
					m <- vegan::decostand(m, "max", 2)
					m <- vegan::decostand(m, "total", 1)
				} else {
					m <- vegan::decostand(m, stand)
				}
			} else {
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
    	m <- .cast(x, mode = 2)
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
    	m <- .cast(x, mode = 3)
   		if (mode == "R") m <- t(m)
   		storage.mode(m) <- "integer"
   		return(invisible(m))    	
    }
)	

#if (!isGeneric("as.matrix")) {
#	setGeneric("as.logical")
#}
#if (!isGeneric("rowSums")) {
setGeneric("as.matrix",
	function (x, ...)
	standardGeneric("as.matrix"))
#}
setMethod("as.matrix",
    signature(x = "Vegsoup"),
    function (x, typeof, ...) {
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
as.array.Vegsoup <- as.matrix.Vegsoup <-
	function (x, ...) as.matrix(x, ...) # as(x, "matrix")

setMethod("as.vector",
	signature(x = "Vegsoup", mode = "missing"), # 
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
	function (x, ...)
	standardGeneric("indices"))	
#}	
setMethod("indices",
	signature(x = "Vegsoup"), # 
	  function (x, typeof) {
    	if (missing(typeof)) typeof <- "numeric"    		
    	TYPEOF <- c("character", "numeric", "logical")
    	typeof <- match.arg(typeof, TYPEOF)
    	
		sc <- coverscale(x)
		al <- file.path(Species(x)$abbr, Species(x)$layer, fsep = "@")
		l <- Species(x)$layer	
		pl <- Species(x)$plot
		upl <- unique(pl)
		
		#	resort to layer, copied from .cast()
		if (length(Layers(x)) > 1) {	

			al <- unique(as.vector(unlist(
				sapply(Layers(x), function (y) al[l == y] ))))
		}
		ual <- unique(al)	
		
		if (typeof == "numeric" & !is.null(sc@codes)) {
			cv <- as.numeric(as.character(
				factor(Species(x)$cov, levels = sc@codes, labels = sc@lims)
				))
		}
		if (typeof == "character") {
			cv <- Species(x)$cov
		}
		if (typeof == "logical") {
			cv <- rep(1, nrow(Species(x)))
		}
		i <- match(al, ual)
		j <- as.integer(ordered(pl, levels = upl))
		list(i = i, j = j, x = cv, dimnames = list(upl, ual))
		}
)

#	coerce to sparse matrix
#	very basic!
setAs(from = "Vegsoup", to = "sparseMatrix",
	def = function (from) {
		require(Matrix)
		ij <- indices(from)
		res <- sparseMatrix(ij$i, ij$j, x = as.integer(ij$x),
			dimnames = ij$dimnames)
		res
	}
)

setAs(from = "Vegsoup", to = "dsparseMatrix",
	def = function (from) {
		require(Matrix)
		ij <- indices(from)
		res <- sparseMatrix(ij$i, ij$j, x = ij$x,
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
