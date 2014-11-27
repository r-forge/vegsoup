#if (!isGeneric("compress")) {
setGeneric("compress",
	function (x, ...)
	standardGeneric("compress"))
#}

setMethod("compress",
    signature(x = "Vegsoup"),
    function (x, retain) {
		coverscale(x) <- "pa"
		
		if (!missing(retain)) {
			j <- match(retain, names(x))
			if (any(j))	x@sites <- data.frame(
				compress = Sites(x)[, j],
				row.names = rownames(Sites(x))) # rownames(x)
		}
		else {
			x@sites <- data.frame(
				compress = rep(TRUE, nrow(x)),
				cols = ncol(x),
				row.names = rownames(Sites(x)))  # rownames(x)
		}
		
		x <- Layers(x, collapse = "0l")
		x@taxonomy <- Taxonomy(x)[c("abbr", "taxon")]
	return(x)	
    }
)
