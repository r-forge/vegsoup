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
#	subset = c("Warnstorfia", "Philonotis")
.taxon.Taxonomy <- function (x, subset, ...) {
	#	subset ="Carex"
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
				tmp[j[[i]]] <- tmp[j[[i]]] + 1
			}
			j <- which(tmp == max(tmp))			
		}	
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