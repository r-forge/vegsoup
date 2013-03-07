shape.species <- function (obj) {
	if (!inherits(obj, "Vegsoup")) {
		stop("only defined for Vegsoup* objects")
	}
	spc <- Species(obj)
	#	data.frame to store results
	res <- as.data.frame(
    	     matrix("",
        	   ncol = length(Layers(dta)) + 2, # we need 2 more columns
	           nrow = sum(richness(dta, "sample"))),
    	   stringsAsFactors = FALSE)
	names(res) <- c("plot", "abbr", Layers(dta))

	res$plot <- rep(rownames(dta), richness(dta, "sample"))
	res$abbr <- unlist(sapply(rownames(dta), function (x) {
		Taxonomy(dta[rownames(dta) == x, ])$abbr}))
	#	slow
	for (i in 1:nrow(res)) {
		tmp <- res[i, ]
	    sel <- tmp$plot == spc$plot & tmp$abbr == spc$abbr
   		res[i, match(spc[sel, 3], names(res))] <- spc[sel, 4]
	}
	return(invisible(res))
}