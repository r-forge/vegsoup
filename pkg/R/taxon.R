setGeneric("taxon",
	function (x, ...)
		standardGeneric("taxon")
)

setMethod("taxon",
	signature = "Vegsoup",
	function (x, taxon = NULL) {
		if (!is.null(taxon))
			taxon(x)[ grep(taxon, taxon(x)) ]	
		else
			taxonomy(x)$taxon
	}
)	

.taxon.Taxonomy <- function (x, subset, ...) {
	allargs <- list(...)
	if (any(names(allargs) == "invert")) {
		#	invert argument to grep needs extra handling!
		invert <- allargs$invert
	} else {
		invert = FALSE	
	}
	if (length(subset) < 1) {
		stop("not a single species given", call. = FALSE)
	}
	
	if (is.logical(subset)) {
		stopifnot(length(subset) == length(taxon(x)))
		j <- taxonomy(taxonomy(x))[subset, 1]
	}
	if (is.numeric(subset)) {
		if (length(unique(subset)) != length(subset)) {
			stop("numeric index must be unique, duplicated items: ",
				paste(subset[duplicated(subset)], collapse = " "), call. = FALSE)
		}
		j <- taxonomy(taxonomy(x))[subset, 1]
	}
	if (is.character(subset)) {
		xx <- taxon(x)
		j <- sapply(subset, simplify = FALSE, USE.NAMES = FALSE,
			FUN = function (x) grep(pattern = x, x = xx, fixed = TRUE))
		j <- sapply(subset, simplify = FALSE, USE.NAMES = FALSE,
			FUN = function (x, ...) grep(pattern = x, x = xx, ...), ...)
		if (!invert) {
			j <- sort(unlist(j))
		}
		else {
			tmp <- rep(0, length(xx))
			for (i in seq(along = j)) {
				tmp[ j[[i]] ] <- tmp[ j[[i]] ] + 1
			}
			j <- which(tmp == max(tmp))
		}	
		j <- taxonomy(taxonomy(x))[ j, 1 ]
	}
	jj <- colnames(x)
	j <- unlist(sapply(j, simplify = FALSE, USE.NAMES = FALSE,
		FUN = function (x) grep(x, jj)))
	
	res <- x[, j]
	res

}

#if (!isGeneric("subset")) {
setGeneric("subset",
	function (x, ...)
	standardGeneric("subset")
)
#}

setMethod("subset",
	signature = "Vegsoup",
	function (x, subset, drop = TRUE, ...) {
		r <- .taxon.Taxonomy(x, subset, ...)
		if (!drop) r <- x[ rownames(r), ]
		return(r)
	}
)