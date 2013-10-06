setGeneric("taxon",
	function (x)
		standardGeneric("taxon")
)

setMethod("taxon",
	signature = "Vegsoup",
	function (x) {
		Taxonomy(x)$taxon	
	}
)	

.taxon.Taxonomy <- function (x, subset, ...) {
	#	subset ="Carex"
	if (length(subset) < 1) {
		stop("not a single species given", call. = FALSE)
	}
	
	if (is.logical(subset)) {
		stopifnot(length(subset) == length(taxon(x)))
		j <- Taxonomy(x)[subset, 1, drop = TRUE]	
	}
	if (is.numeric(subset)) {
		if (length(unique(subset)) != length(subset)) {
			stop("numeric index must be unique, duplicated items: ",
				paste(subset[duplicated(subset)], collapse = " "), call. = FALSE)
		}
		j <- Taxonomy(x)[subset, 1, drop = TRUE]
	}
	if (is.character(subset)) {
		xx <- taxon(x)
		j <- unlist(sapply(subset, simplify = FALSE, USE.NAMES = FALSE,
			FUN = function (x, ...) grep(pattern = x, x = xx, ...), ...))
		j <- Taxonomy(x)[j, 1, drop = TRUE]
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
	function (x, subset, ...) {
		.taxon.Taxonomy(x, subset, ...)	
	}
)