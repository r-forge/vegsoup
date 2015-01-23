#	from list to Coverscale
setAs("list", "Coverscale", def = function (from) {
	#	ordinal
	if (!is.null(from[[2]]) & !is.null(from[[3]])) {
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = as.character(from[[2]]),
			lims = as.numeric(from[[3]]))
	}		
	#	continous
	if (is.null(from[[2]]) & is.null(from[[3]])) {
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = NULL,
			lims = NULL
			)			
	}
	return(res)
})

#	set S3 class
#setOldClass("coenoflex")
#	from coenoflex to Vegsoup
#as.Vegsoup.coenoflex <- function (obj) {

#	spc <- obj$veg
#	sts <- obj$site
#	ns <- ncol(spc)
#	np <- nrow(spc)

	#	groome decimals
#	spc[spc > 0 & spc <= 0.1] <- 0.1 #! document this

	#	coenoflex behaves unexpected for low numbers of 'numplt' and 'numspc'
	#	so we need to cure empty species
#	test <- colSums(spc) == 0
#	if (any(test)) {
#		for (i in which(test)) {
#			spc[sample(1:nrow(spc), size = 1), i] <- 0.1
#		}
#	}	
	#	and empty sites
#	test <- rowSums(spc) == 0
#	if (any(test)) {
#		for (i in which(test)) {
#			spc[i, sample(1:ncol(spc), size = 1)] <- 0.1
#		}
#	}	
	
	#	meaningful names
#	abbr <- sprintf(paste0("spc%0", nchar(ns), ".0f"), 1:ns)
#	taxon <- sprintf(paste0("Species %0", nchar(ns), ".0f"), 1:ns)
#	plot <- sprintf(paste0("plt%0", nchar(np), ".0f"), 1:np)
	
	#	row-wise index to as.vector()
#	ij <- c(t(matrix(seq_len(np * ns), nrow = np, ncol = ns)))
	#	pointer to non-zero values
#	z <- spc[ij] != 0
	
#	spc <- matrix(c(
#		rep(plot, each = ns)[z],	# plot
#		rep.int(abbr, np)[z],		# abbr
#		rep("0l", length(which(z))),# layer
#		round(spc[ij[z]], 1)),		# cov
#		ncol = 4, nrow = table(z)[2])
	
#	sts <- stackSites(data.frame(plot = plot, sts))
	
#	txa <- taxonomy(cbind(abbr, taxon))
	
#	res <- Vegsoup(spc, sts, txa, "percentage")
	
#	return(res)

#}

#setAs(from = "coenoflex", to = "Vegsoup",
#	def = function (from) {
#		as.Vegsoup.coenoflex(from)
#	}
#)

#	set S3 class
setOldClass("data.list")

#	from Vegsoup to data.list
".as.data.list.Vegsoup" <- function (obj) {
	#	Imports:
	#	require(multitable)

	xx <- species(species(obj)) #! get slot data
	names(xx)[4] <- "abundance"
	scale <- coverscale(obj)
	
	#	cover transformation
	if (!is.null(scale@codes)) {
		xx$abundance <- as.numeric(as.character(
			factor(xx$abundance, levels = scale@codes, labels = scale@lims)))
		if (any(is.na(xx$abundance))) {
			stop("cover scale codes do not match data")
		}
	}
	if (is.null(scale@codes)) {
		xx$cov <- as.numeric(xx$cov)
	}
	
	yy <- data.frame(plot = rownames(obj), sites(obj))
		
	zz <- taxonomy(taxonomy(obj)) # slot data

	#xyz <- data.frame(plot = rownames(obj), coordinates(obj))
		
	l <- list(xx[, c(1,2,4)], xx[, c(1,2,3)], yy, zz)
	res <- multitable::dlcast(l, fill = c(0, "", NA, NA))
	
	return(res)
}

setAs(from = "Vegsoup", to = "data.list",
	def = function (from) {
		.as.data.list.Vegsoup(from)
	}
)

#	as.mefa.Vegsoup <- fucntion(obj) as(obj, "data.list")

#	set S3 class
setOldClass("mefa")

#	from Vegsoup to mefa
".as.mefa.Vegsoup" <- function (obj) {
	x <- species(species(obj)) #! get slot data
	if (is.ordinal(coverscale(obj))) {
		x$cov = as.numeric(as.character(factor(species(obj)$cov,
					levels = coverscale(obj)@codes,
					labels = coverscale(obj)@lims)))
	}
	if (is.continuous(coverscale(obj))) {
		x$cov <- as.numeric(x$cov)	
	}			
	return(mefa(stcs(x[, c(1,2,4,3)]), sites(obj), taxonomy(obj), nested = FALSE))
}

setAs(from = "Vegsoup", to = "mefa",
	def = function (from) {
		.as.mefa.Vegsoup(from)
	}
)

#	as.mefa.Vegsoup <- fucntion(obj) as(obj, "mefa")