combine <- function (x, y, z) {

stopifnot(inherits(x, "Vegsoup"))
stopifnot(is.list(y))
	
X <- species(species(x))
Z <- taxonomy(taxonomy(x))
Y <- coverscale(x)

if (missing(z)) {
	z <- list(Z$abbr[ match(y[[2]], Z$taxon) ], y[[2]])
} else {
	stopifnot(is.list(z))
}

#	ensure names if not given
names(y) <- c("from", "to")
names(z) <- c("abbr", "taxon")

#	valid strings
z$abbr <- make.names(z$abbr)

#	test if y$from exists in x

#	more precise and/or rigorious tests
#stopifnot(!any(z$abbr == abbr(x)))

#	convert original abundance scale to numeric to allow calculations
if (is.ordinal(coverscale(x))) {
	X$cov <- ordered(X$cov, levels = Y@codes, labels = Y@lims)
	X$cov <- as.numeric(as.character(X$cov))
} else {
	X$cov <- as.numeric(X$cov)
}

from <- y$from
to <- y$to

Z.from <- match(from, Z$taxon)

if (any(is.na(Z.from)))
	stop(paste0(from[is.na(Z.from)], collapse = ", "), " not in data set")
	
Z.to <- match(to, Z$taxon) # no test yet, we use argument z

Zi <- Z$abbr[Z.from]
Xi <- which(rowSums(sapply(Zi, function (i) i == X$abbr)) > 0)

#	the species
#	select subset of species to combine (c, Xc) or retain (r, Xr)
Xc <- X[ Xi, ]
Xr <- X[-Xi, ]

#	assign new abbr in subset
Xc$abbr <- z$abbr

#	sum duplicates
if (any(duplicated(Xc[, -4]))) {
	Xc <- aggregate(cov ~ plot + abbr + layer, data = Xc, FUN = "sum")
}	

#	merge subsets
X <- rbind(Xr, Xc)

#	explicit ordering!
X <- X[order(X$plot, X$layer, X$abbr), ]

#	back convert to original abundance scale if it was character
if (is.ordinal(coverscale(x))) {
	X$cov <- as.character(cut(X$cov, breaks = c(0, Y@lims), labels = Y@codes))
}
	
X <- species(X)

#	the taxonomy
#	select subset of species to combine (c) or retain (r)
Zr <- Z[ -Z.from, ] # delete from

#	in case if y$from had more then 1 entry, if not because we dropped it already above

#	do we need to add a new taxon
if (any(z$taxon == Zr$taxon) == FALSE) {
	Zc <- Z[0, ]
	Zc[1, ]$abbr <- z$abbr
	Zc[1, ]$taxon <- z$taxon

	Z <- rbind(Zr, Zc)
} else {
#	Zr <- Zr[ z$taxon != Zr$taxon, ]
	Z <- Zr
}
#	explicit ordering!
Z <- Z[order(Z$abbr), ]

#	check if we need to drop a taxon
test <- length(unique(Z$abbr)) != nrow(Z)
if (test) {
	message("error")
}
Z <- taxonomy(Z)

#	assign object slots
x@species <- X
x@taxonomy <- Z

return(x)
}