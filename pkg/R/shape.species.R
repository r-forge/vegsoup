shape.species <- function (obj) {
	if (!inherits(obj, "Vegsoup")) {
		stop("only defined for Vegsoup* objects")
	}
	spc <- Species(obj)
	#	data.frame to store results
	res <- as.data.frame(
    	     matrix("",
        	   ncol = length(Layers(obj)) + 2, # we need 2 more columns
	           nrow = sum(richness(obj, "sample"))),
    	   stringsAsFactors = FALSE)
	names(res) <- c("plot", "abbr", Layers(obj))

	res$plot <- rep(rownames(obj), richness(obj, "sample"))
	res$abbr <- unlist(sapply(rownames(obj), function (x) {
		Taxonomy(obj[rownames(obj) == x, ])$abbr}))
	#	slow
	for (i in 1:nrow(res)) {
		tmp <- res[i, ]
	    sel <- tmp$plot == spc$plot & tmp$abbr == spc$abbr
   		res[i, match(spc[sel, 3], names(res))] <- spc[sel, 4]
	}
	return(invisible(res))
}