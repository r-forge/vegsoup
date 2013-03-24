#	from list to Coverscale
setAs("list", "Coverscale", def = function (from) {
	#	ordinal
	if (!is.null(from[[2]]) & !is.null(from[[3]])) {
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = as.character(from[[2]]),
			lims = as.numeric(from[[3]])						
			)
	}		
	#	continous
	if (is.null(from[[2]]) & is.null(from[[3]])) { # 			
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = NULL,
			lims = NULL						
			)			
	}
	return(res)
})

#	set S3 class
setOldClass("coenoflex")
#	from coenoflex to Vegsoup
as.Vegsoup.coenoflex <- function (obj) {

	require(coenoflex)
	
	spc <- obj$veg
	sts <- obj$site
	
	#	groome decimals
	spc[spc > 0 & spc <= 0.2] <- 0.2 #! document this
	spc <- round(spc, 1) # ... and that

	#	coenoflex behaves unexpected for low numbers of 'numplt' and 'numspc'
	#	so we need to cure empty species
	test <- colSums(spc) == 0
	if (any(test)) {
		for (i in which(test)) {
			spc[sample(1:nrow(spc), size = 1), i] <- 0.1
		}
	}	
	#	and empty sites
	test <- rowSums(spc) == 0
	if (any(test)) {
		for (i in which(test)) {
			spc[i, sample(1:ncol(spc), size = 1)] <- 0.1
		}
	}	
	
	abbr <- gsub(" ", "0", paste0("spc", format(1:ncol(spc))))
	taxon <- gsub("spc", "species", abbr)
	plot <- gsub(" ", "0", paste0("plt", format(1:nrow(spc))))
		
	spc <- cbind(
		rep(plot, times = length(abbr)),	# plot
		rep(abbr, each = length(plot)),		# abbr
		"0l",								# layer
		as.vector(spc))						# cov
		
	spc <- spc[spc[,4] != 0, ]
	spc <- species(spc[order(spc[,1], spc[,2]), ])
	
	sts <- stack.sites(data.frame(plot = plot, sts))
	
	txa <- taxonomy(cbind(abbr, taxon))
	
	res <- Vegsoup(spc, sts, txa, "percentage")
	
	return(res)

}

setAs(from = "coenoflex", to = "Vegsoup",
	def = function (from) {
		as.Vegsoup.coenoflex(from)
	}
)

#	set S3 class
setOldClass("data.list")

#	from Vegsoup to data.list
as.data.list.Vegsoup <- function (obj) {
	require(multitable)

	xx <- Species(obj)
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
	
	yy <- data.frame(plot = rownames(obj), Sites(obj))
		
	zz <- Taxonomy(obj)

	#xyz <- data.frame(plot = rownames(obj), coordinates(obj))
		
	l <- list(xx[, c(1,2,4)], xx[, c(1,2,3)], yy, zz)
	res <- dlcast(l, fill = c(0, "", NA, NA))
	
	return(res)
}

setAs(from = "Vegsoup", to = "data.list",
	def = function (from) {
		as.data.list.Vegsoup(from)
	}
)

#	set S3 class
setOldClass("mefa")

#	from Vegsoup to mefa
as.mefa.Vegsoup <- function (obj) {
	x <- Species(obj)
	if (is.ordinal(coverscale(obj))) {
		x$cov = as.numeric(as.character(factor(Species(obj)$cov,
					levels = coverscale(obj)@codes,
					labels = coverscale(obj)@lims)))
	}
	if (is.continuous(coverscale(obj))) {
		x$cov <- as.numeric(x$cov)	
	}			
	return(mefa(stcs(x[, c(1,2,4,3)]), Sites(obj), Taxonomy(obj), nested = FALSE))
}

setAs(from = "Vegsoup", to = "mefa",
	def = function (from) {
		as.mefa.Vegsoup(from)
	}
)