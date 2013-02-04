#	from Vegsoup 0.1_6
#	artefacts of Vegsoup-BaseMethods

	if (missing(col.names)) {
		col.names <- list(
			x = c("plot", "abbr", "layer", "cov"),
			y = c("plot", "variable", "value"),
			z = c("abbr", "taxon"))
	} else {	
		if (!is.list(col.names)) {
			stop("col.names must be a list")
		} else {
			if (length(col.names) != 3) {
				stop("col.names must be a list of character and length 3")
			} else {
				names(col.names) <- c("x", "y", "z")
				print(col.names)			
			}
		}
	}
	
setMethod("summary",
    signature(object = "Vegsoup"),
	function (object) {
		cat("an object of class", class(object))
		if (is.null(Taxonomy(object)) || nrow(Taxonomy(object)) != length(Abbreviation(object))) {
			cat("\n taxonomy lookup table missing or uncomplete")
		} else {
			cat("\n taxonomy lookup table supplied and complete")
		}
		cat("\n  proj4string:")
		print(proj4string(object))
		cat("\n  bbox:\n")
		print(bbox(object))
	}
)	