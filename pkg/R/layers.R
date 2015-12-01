#	see Ewald 2013, Tuexina 33
#l <- c(75,15,55)
#names(ll) <- c("tl1", "tl2", "sl")
#ll <- l/100

#r0 <- ((ll[1] + ll[2]) - ll[1] * ll[2]) * 100
#(r0/100 + ll[3]) - (r0/100 * ll[3])

#	layers method
".layers.Vegsoup" <- function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE) {
	
	#	return object if mandatory arguments are missing
	if (missing(collapse) & missing(aggregate))
		return(obj@layers)	
	
	#	return object if there is nothing to do
	if (length(obj@layers) < 2) {
		if (verbose) {
			message("obj has already only a single layer: ",
			obj@layers, ", set to: ", collapse)
		}	
		species(obj)$layer <- collapse
		obj@layers <- collapse
		return(obj)
	}
	
	#	check 'aggregate' argument or set defaults
	if (missing(aggregate)) aggregate <- "layer" else aggregate <- match.arg(aggregate)
	
	#	'collapse' missing or of length 1 (full collapse)
	if (missing(collapse) || length(collapse) == 1) {
		if (verbose) message("collapse to a single layer")
		if (missing(collapse)) { L <- "0l" } else { L <- collapse }
		L <- rep(L, length(layers(obj)))
	}
	else {
	#	'collapse' not of correct length
		stopifnot(length(collapse) <= length(layers(obj)))
	#	'collapse' statisfies basic assuptions
		stopifnot(length(collapse) == length(layers(obj)))
		L <- collapse
	}
	
	#	check 'dec' argument	
	if (is.ordinal(obj)) { 		
		if (dec == 0 & trunc(min(coverscale(obj)@lims)) == 0) {
			for (dec in 0:5) {
				if (round(min(coverscale(obj)@lims), dec) != 0) break
			}
			if (verbose) {
				message("set dec to ", dec,
					", min of coverscale(obj)@lims is ",
					min(coverscale(obj)@lims), "!")
			}	
		}
	}

	ii <- rownames(obj)	# original order
	X <- species(species(obj))
	Y <- coverscale(obj)
	
	L <- matrix(c(layers(obj), L),
		ncol = 2, nrow = length(layers(obj)),
		byrow = FALSE, dimnames = list(NULL, c("original", "collapsed")))
		
	if (verbose) print(L)
	
	#	test collapse vector, especially concerning NAs
	nas <- is.na(L[, 2])
	if (any(nas)) {
		if (verbose) message("NA in collapse, dropped all species on these layers")
		ld <- L[nas, 1] # layers to drop
		L <- L[!nas, ]
		#	if we loose dimension
		if (!is.matrix(L)) L <- t(L)
		#	drop all occurences on these layers
		X <- X[!X$layer %in% ld, ]
	
		if (length(rownames(obj@sites)) > length(unique(X$plot))) {
			if (verbose) message("also some plots will be dropped")
		}
	}	
	
	X$layer <- factor(X$layer)
	levels(X$layer) <- L[match(levels(X$layer), L[, 1]), 2]
	X$layer <- as.character(X$layer)
	
	#	convert original abundance scale to numeric to allow calculations
	if (is.ordinal(coverscale(obj))) {
		X$cov <- ordered(X$cov, levels = Y@codes, labels = Y@lims)
		X$cov <- as.numeric(as.character(X$cov))
	}
	else {
		X$cov <- as.numeric(X$cov)
	}
	
	#	aggregate layers
	X  <- switch(aggregate,
	   mean = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = mean)
	}, min = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = min)				
	}, max = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = max)
	}, sum = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = sum)
	}, layer = {
		if (is.ordinal(coverscale(obj))) {
		aggregate(cov ~ plot + abbr + layer, data = X,
				FUN = function (x) {
					round((1 - prod(1 - x / max(Y@lims))) * max(Y@lims), dec)
				})
		}
		#	critcal! returns 0 for e.g. 0.1 if dec = 0
		else {
			aggregate(cov ~ plot + abbr + layer, data = X,
				FUN = function (x) {
					round((1 - prod(1 - x / 100)) * 100, dec)	
					})
		}		
	})
	
	#	explicit ordering!
	X <- X[order(X$plot, X$layer, X$abbr), ]
	
	if (!is.null(Y@codes)) {
		if (any(max(X$cov) > max(Y@lims))) {
			message("\nreduced maximum aggregated abundance value to fit into limits: ",
				min(Y@lims)[1], " to ", max(Y@lims))
			X$cov[X$cov > max(Y@lims)] <- max(Y@lims)
		}
	}

	#	round to dec, do we really need this?
	if (dec == 0)
		X$cov <- ceiling(X$cov)
	else
		X$cov <- round(X$cov, dec)
	
	#	back convert to original abundance scale if it was character
	if (is.ordinal(coverscale(obj))) {
		X$cov <- as.character(cut(X$cov, breaks = c(0, Y@lims), labels = Y@codes))
	}
	
	#	use replace method (keeps order of obj)
	species(obj) <- species(X)
		
	return(invisible(obj))
}

setGeneric("layers",
	function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 1, verbose = FALSE)
	standardGeneric("layers")
)

setGeneric("layers<-", function (obj, value)
	standardGeneric("layers<-")
)

setMethod("layers",
   signature(obj = "Vegsoup"),
	.layers.Vegsoup
)

setReplaceMethod("layers",
	signature(obj = "Vegsoup", value = "ANY"),
	function (obj, value) {
		if (length(value) != length(layers(obj))) {
			stop("length of value does not match length layers of object")
		}
		if (any(!layers(obj) %in% value)) {
			stop("items of value do not match layers of object",
				"\n use layers(obj, collapse = value)") 
		}
		obj@layers <- value
		if (inherits(obj, "VegsoupPartitionFidelity")) {
			#	resort what is independent of as.matrix methods
			i <- colnames(obj)
			obj@stat <- obj@stat[i, ]
			#obj@lowerCI <- obj@lowerCI[i, ]
			#obj@upperCI <- obj@upperCI[i, ]
			obj@fisher.test <- obj@fisher.test[i, ]
		}
		return(obj)
	}
)

#	return just the layer columns from species(obj)
setGeneric("layer",
	function (obj, ...)
		standardGeneric("layer"))
setMethod("layer",
   signature(obj = "Vegsoup"),
	function (obj, ...) species(obj)$layer
)

#	return just the layer columns from species(obj)
if (!isGeneric("layernumber")) {
setGeneric("layernumber",
	function (obj, ...)
		standardGeneric("layernumber"))
}		
setMethod("layernumber",
   signature(obj = "Vegsoup"),
	function (obj, ...) as.numeric(ordered(splitAbbr(obj)$layer, layers(obj)))
)
