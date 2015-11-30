plainsoup <- function (m, coverscale, layer = "0l", verbose = TRUE) {
	if (missing(coverscale)) {
		stop("please supply appropiate cover scale of data")
	} else {
		if (is.character(coverscale) & !inherits(coverscale, "Coverscale")) {
			coverscale <- Coverscale(coverscale)	
		} else {
			stopifnot(inherits(coverscale, "Coverscale"))	
		}		
	}	
			
	x <- as.matrix(m)
	x <- data.frame(
		abbr = colnames(x),
		layer = c(rep(layer, ncol(x))),
		taxon = c(rep(NA, ncol(x))),
		t(x))
	x <- stackSpecies(x, verbose = verbose)[, 1:4]
	
	z <- taxonomy(data.frame(abbr = unique(abbr(x)), taxon = unique(abbr(x))))
	
	y <- data.frame(
		plot = unique(x$plot),
		variable = "X1",
		value = 0)
	y <- sites(y)	

	r <- Vegsoup(x, y, z, coverscale)
}