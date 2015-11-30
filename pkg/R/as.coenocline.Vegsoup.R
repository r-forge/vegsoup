as.coenocline.Vegsoup <- function (obj) {

	if (attr(obj, "expectation"))
		stop("expectation must be FALSE")

	#	species
	X <- obj[]
	
	#	ensure percentage bounds
	if (any(X > 100)) {
		X <- (X / max(X)) * 100
	}
	
	
	#	sites
	if (inherits(attr(obj, "locations"), "matrix")) {
		Y <- as.data.frame(attr(obj, "locations"))
		names(Y) <- paste0("gradient", 1:ncol(Y))
	} else {
		Y <- data.frame(gradient1 = attr(obj, "locations"))	
	}
	
	j <- ncol(X) # number of species
	i <- nrow(X) # number of plots

	#	cure empty species
	test <- colSums(X) == 0
	if (any(test)) {
		for (k in which(test)) {
			X[sample(1:nrow(X), size = 1), k] <- 1
		}
	}	
	#	... and empty sites
	test <- rowSums(X) == 0
	if (any(test)) {
		for (k in which(test)) {
			X[k, sample(1:ncol(X), size = 1)] <- 1
		}
	}	
	
	#	meaningful names
	abbr <- sprintf(paste0("spc%0", nchar(j), ".0f"), 1:j)
	taxon <- sprintf(paste0("Species %0", nchar(j), ".0f"), 1:j)
	plot <- sprintf(paste0("plt%0", nchar(i), ".0f"), 1:i)
	
	#	row-wise index to as.vector()
	ij <- c(t(matrix(seq_len(i * j), nrow = i, ncol = j)))
	#	pointer to non-zero values
	z <- X[ij] != 0
	
	X <- matrix(c(
		rep(plot, each = j)[z],	 # plot
		rep.int(abbr, i)[z],		 # abbr
		rep("0l", length(which(z))), # layer
		round(X[ij[z]], 1)),		 # cov
		ncol = 4, nrow = table(z)[2])
	
	X <- species(X)
	
	Y <- stackSites(data.frame(plot = plot, Y))
	
	Z <- taxonomy(cbind(abbr, taxon))
	
	res <- Vegsoup(X, Y, Z, "percentage")
	
	return(res)

}

setOldClass("coenocline")

setAs(from = "coenocline", to = "Vegsoup",
	def = function (from) {
		as.coenocline.Vegsoup(from)
	}
)