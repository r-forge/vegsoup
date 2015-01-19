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
			if (any(!is.na(j)))	{
				x@sites <- data.frame(
					compress = rep(TRUE, nrow(x)),
					Sites(x)[, j[!is.na(j)], drop = FALSE],
					row.names = rownames(Sites(x)))
			}
			else {
				x@sites <- data.frame(
					compress = rep(TRUE, nrow(x)),
					cols = ncol(x),
					row.names = rownames(Sites(x)))
			}
		}
		else {
			x@sites <- data.frame(
				compress = rep(TRUE, nrow(x)),
				cols = ncol(Sites(x)),
				row.names = rownames(Sites(x)))     
		}
		
		x <- Layers(x, collapse = "0l")
		
		z <- taxonomy(taxonomy(x)) 
		j <- c("abbr", "taxon", "family", "level")
		j <- match(j, names(z))
		z <- z[, j[!is.na(j)]]

		x@taxonomy <- taxonomy(z)
	return(x)	
    }
)
